open Token
open Types

let rec split_by_token token left = function
  | [] -> List.rev left, []
  | x :: right when x = token -> List.rev left, right
  | x :: right -> split_by_token token (x :: left) right

let split_by_token token token_list = split_by_token token [] token_list

(* parse_expression and auxiliary functions *)

let rec parse_expression tokens =
    match low_pr_expr_outer tokens with
    | result, [] -> result
    | _ -> raise Invalid_expression

and low_pr_expr_outer tokens =
    let (left, rest) = high_pr_expr_outer tokens in low_pr_expr_inner left rest

and high_pr_expr_outer tokens = 
    let (left, rest) = parse_term tokens in high_pr_expr_inner left rest

and low_pr_expr_inner left = function
    | PLUS :: rest ->
        let (right, rest) = high_pr_expr_outer rest in
        low_pr_expr_inner (BinOp(Add, left, right)) rest
    | MINUS :: rest ->
        let (right, rest) = high_pr_expr_outer rest in
        low_pr_expr_inner (BinOp(Sub, left, right)) rest
    | other -> (left, other)

and high_pr_expr_inner left = function
    | STAR :: rest ->
        let (right, rest) = parse_term rest in
        high_pr_expr_inner (BinOp(Mul, left, right)) rest
    | SLASH :: rest ->
        let (right, rest) = parse_term rest in
        high_pr_expr_inner (BinOp(Div, left, right)) rest
    | other -> (left, other)

and parse_term = function
    | [] -> raise Invalid_expression
    | INT n :: rest -> Int n, rest
    | ID name :: rest -> parse_call name [] rest
    | MINUS :: rest ->
        let (expr, rest) = parse_term rest in Neg(expr), rest
    | LP :: rest ->
        let (expr, rest) = low_pr_expr_outer rest in
        (match rest with
        | RP :: rest -> expr, rest
        | _ -> raise Invalid_expression)
    | _ -> raise Invalid_expression

and parse_call name args rest =
    match rest with
    | ID x :: rest -> parse_call name ((Var x) :: args) rest
    | INT x :: rest -> parse_call name ((Int x) :: args) rest
    | _ -> match args with
        | [] -> Var name, rest
        | args2 -> Call(name, List.rev args2), rest

(* parse_boolean_expression and auxiliary functions *)

let comparison_of_token_opt = function
    | EQ  -> Some Eq
    | NEQ -> Some Neq
    | LT  -> Some Lt
    | GT  -> Some Gt
    | LEQ -> Some Leq
    | GEQ -> Some Geq
    | _ -> None

let rec parse_boolean_expression left = function
  | [] -> raise Invalid_expression
  | token :: right ->
    match comparison_of_token_opt token with
    | Some cmp -> Comparison(cmp, left |> List.rev |> parse_expression, parse_expression right)
    | None -> parse_boolean_expression (token :: left) (right)

let parse_boolean_expression tokens = parse_boolean_expression [] tokens

(* parse_to_program and auxiliary functions *)

let rec close_paren left balance = function
| [] -> raise Invalid_expression
| LP :: right -> close_paren (LP :: left) (balance + 1) right
| RP :: right -> if balance = 0 then List.rev left, right else close_paren (RP :: left) (balance - 1) right
| token :: right -> close_paren (token :: left) balance right

(* TODO: replace this cringe with returning the rest from parse_expression *)
let close_paren tokens = close_paren [] 0 tokens 

let rec parse_to_program acc = function
    | [] -> List.rev acc, []
    | VAR :: rest ->
        let declaration, rest = parse_declaration [] rest in
        parse_to_program (declaration :: acc) rest
    | WHILE :: rest ->
        let while_stmnt, rest = parse_while rest in
        parse_to_program (while_stmnt :: acc) rest
    | ID name :: rest ->
        (match rest with
        | COLONEQQ :: rest -> 
            let assignment, rest = parse_assignment name rest in
            parse_to_program (assignment :: acc) rest
        | _ ->
            let call, rest = parse_call_stmt name [] rest in
            parse_to_program (call :: acc) rest)

    | IF :: rest ->
        let ite, rest = parse_ite rest in
        parse_to_program (ite :: acc) rest
    | ELSE :: rest -> List.rev acc, ELSE :: rest
    | FI :: rest -> List.rev acc, FI :: rest

    | DONE :: rest -> List.rev acc, rest
    | _ -> raise Invalid_statement

and parse_while tokens =
  let boolean_expression_tokens, statements_tokens = split_by_token DO tokens in
  let boolean_expression = parse_boolean_expression boolean_expression_tokens in
  let statements, rest = parse_to_program [] statements_tokens in
    While(boolean_expression, statements), rest

and parse_assignment name tokens = 
    let statement_tokens, rest = split_by_token SEMICOLON tokens in
      Assignment(name, parse_expression statement_tokens), rest

and parse_declaration acc = function
  | SEMICOLON :: rest -> Declaration (List.rev acc), rest
  | ID name :: rest -> parse_declaration (name :: acc) rest
  | _ -> raise Invalid_statement

and parse_ite tokens = 
    let tokens_of_boolean_expression, then_tokens = split_by_token THEN tokens in
    let boolean_expression = parse_boolean_expression tokens_of_boolean_expression in
    let then_program, rest = parse_to_program [] then_tokens in
    match rest with
    | FI :: rest -> Ite(boolean_expression, then_program, []), rest
    | ELSE :: rest ->
        (let else_program, rest = parse_to_program [] rest in
        match rest with
        | FI :: rest -> Ite(boolean_expression, then_program, else_program), rest
        | _ -> raise Invalid_statement)
    | _ -> raise Invalid_statement

and parse_call_stmt name acc = function
| [] -> raise Invalid_statement
| token :: rest ->
    match token with
    | SEMICOLON -> Call(name, List.rev acc), rest
    | INT x -> parse_call_stmt name (Int x :: acc) rest
    | ID x -> parse_call_stmt name (Var x :: acc) rest
    | LP ->
        let expr_tokens, rest = close_paren rest in
        parse_call_stmt name ((parse_expression expr_tokens) :: acc) rest
    | _ -> raise Invalid_statement

let parse_to_program tokens =
  match parse_to_program [] tokens with
  | result, [] -> result
  | _ -> raise Invalid_statement
