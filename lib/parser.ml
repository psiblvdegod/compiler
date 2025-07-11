open Token
open Types

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
        low_pr_expr_inner (Add(left, right)) rest
    | MINUS :: rest ->
        let (right, rest) = high_pr_expr_outer rest in
        low_pr_expr_inner (Sub(left, right)) rest
    | other -> (left, other)

and high_pr_expr_inner left = function
    | STAR :: rest ->
        let (right, rest) = parse_term rest in
        high_pr_expr_inner (Mul(left, right)) rest
    | SLASH :: rest ->
        let (right, rest) = parse_term rest in
        high_pr_expr_inner (Div(left, right)) rest
    | other -> (left, other)

and parse_term = function
    | [] -> raise Invalid_expression
    | INT n :: rest -> Int n, rest
    | ID name :: rest -> parse_call name [] rest
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

(* parse_comparison and auxiliary functions *)

let rec parse_comparison left = function
  | [] -> raise Invalid_expression
  | token :: right ->
    match token with
    | EQ  -> Eq(left |> List.rev |> parse_expression, parse_expression right)
    | NEQ -> Neq(left |> List.rev |> parse_expression, parse_expression right)
    | LEQ -> Leq(left |> List.rev |> parse_expression, parse_expression right)
    | GEQ -> Geq(left |> List.rev |> parse_expression, parse_expression right)
    | LT  -> Lt(left |> List.rev |> parse_expression, parse_expression right)
    | GT  -> Gt(left |> List.rev |> parse_expression, parse_expression right)
    | other -> parse_comparison (other :: left) (right)
  
let parse_comparison tokens = parse_comparison [] tokens


(* parse_to_program and auxiliary functions *)

let rec parse_to_program result tokens =
  match tokens with
  | [] -> List.rev result, []
  | VAR :: rest ->
    let assignment, rest = parse_assignment rest in
    parse_to_program (assignment :: result) rest
  | WHILE :: rest ->
    let condition, statements, rest = parse_while rest in
    parse_to_program (While(condition, statements) :: result) rest
  | DONE :: rest -> List.rev result, rest
  | _ -> raise Invalid_statement

and parse_while tokens =
  let (condition_tokens, statements_tokens) = split_by_token DO [] tokens in
  let condition = parse_comparison condition_tokens in
  let (statements, rest) = parse_to_program [] statements_tokens in
  condition, statements, rest

and parse_assignment = function
  | ID name :: tokens ->
    (match tokens with
    | COLONEQQ :: tokens -> 
      let statement_tokens, rest = split_by_token SEMICOLON [] tokens in
        Assignment(name, parse_expression statement_tokens), rest
    | _ -> raise Invalid_statement)
  | _ -> raise Invalid_statement

and split_by_token token left = function
  | [] -> List.rev left, []
  | x :: right when x = token -> List.rev left, right
  | x :: right -> split_by_token token (x :: left) right

let parse_to_program tokens =
  match parse_to_program [] tokens with
  | result, [] -> result
  | _ -> raise Invalid_statement
