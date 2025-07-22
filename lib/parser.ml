open Token
open Types

let rec split_by_token token left = function
  | [] -> List.rev left, []
  | x :: right when x = token -> List.rev left, right
  | x :: right -> split_by_token token (x :: left) right

let split_by_token token token_list = split_by_token token [] token_list

let rec expr_prior_4_outer tokens =
  let (left, tokens) = expr_prior_3_outer tokens in expr_prior_4_inner left tokens

and expr_prior_4_inner left = function
  | EQ :: rest ->
      let (right, rest) = expr_prior_3_outer rest in
          expr_prior_4_inner (BinOp(Eq, left, right)) rest
  | NEQ :: rest ->
      let (right, rest) = expr_prior_3_outer rest in
          expr_prior_4_inner (BinOp(Neq, left, right)) rest
  | LEQ :: rest ->
      let (right, rest) = expr_prior_3_outer rest in
          expr_prior_4_inner (BinOp(Leq, left, right)) rest
  | GEQ :: rest ->
      let (right, rest) = expr_prior_3_outer rest in
          expr_prior_4_inner (BinOp(Geq, left, right)) rest
  | LT :: rest ->
      let (right, rest) = expr_prior_3_outer rest in
          expr_prior_4_inner (BinOp(Lt, left, right)) rest
  | GT :: rest ->
      let (right, rest) = expr_prior_3_outer rest in
          expr_prior_4_inner (BinOp(Gt, left, right)) rest
  | other -> (left, other)

and expr_prior_3_outer tokens =
  let (left, tokens) = expr_prior_2_outer tokens in expr_prior_3_inner left tokens

and expr_prior_3_inner left = function
  | PLUS :: rest ->
      let (right, rest) = expr_prior_2_outer rest in
          expr_prior_3_inner (BinOp(Add, left, right)) rest
  | MINUS :: rest ->
      let (right, rest) = expr_prior_2_outer rest in
          expr_prior_3_inner (BinOp(Sub, left, right)) rest
  
  (* TODO? : CAT, AND, OR *)
  | other -> (left, other)

and expr_prior_2_outer tokens =
  let (left, tokens) = expr_prior_1 tokens in expr_prior_2_inner left tokens

and expr_prior_2_inner left = function
  | STAR :: rest ->
      let (right, rest) = expr_prior_1 rest in
          expr_prior_2_inner (BinOp(Mul, left, right)) rest
  | SLASH :: rest ->
      let (right, rest) = expr_prior_1 rest in
          expr_prior_2_inner (BinOp(Div, left, right)) rest
  | other -> (left, other)

and expr_prior_1 = function
    | [] -> raise Invalid_expression

    | INT n :: rest -> Int n, rest
    | ID name :: rest -> Var (Id name), rest (* TODO : parse call *)
    | TRUE :: rest -> Bool true, rest
    | FALSE :: rest -> Bool false, rest
    | MINUS :: rest ->
        let (expr, rest) = expr_prior_1 rest in UnOp(Neg, expr), rest
    | LP :: rest ->
        let (expr, rest) = expr_prior_4_outer rest in
        (match rest with
        | RP :: rest -> expr, rest
        
        | _ -> raise Invalid_expression)
    | _ -> raise Invalid_expression

let parse_expr_with_rest tokens = expr_prior_4_outer tokens

let parse_expr_raise_if_rest tokens =
  match parse_expr_with_rest tokens with
  | expr, [] -> expr
  | _ -> raise Invalid_expression

let rec parse_to_program acc = function
    | [] -> List.rev acc, []

    | VAR :: rest ->
        let declaration, rest = parse_declaration [] rest in
        parse_to_program (declaration :: acc) rest
    | WHILE :: rest ->
        let while_stmnt, rest = parse_while rest in
        parse_to_program (while_stmnt :: acc) rest
    | ID name :: COLONEQQ :: rest ->
        let assignment, rest = parse_assignment name rest in
            parse_to_program (assignment :: acc) rest

    | IF :: rest ->
        let ite, rest = parse_ite rest in
        parse_to_program (ite :: acc) rest
    | ELSE :: rest -> List.rev acc, ELSE :: rest
    | FI :: rest -> List.rev acc, FI :: rest

    | DONE :: rest -> List.rev acc, rest
    
    | _ -> raise Invalid_statement

and parse_declaration acc = function
    | [] -> raise Invalid_statement
    | SEMICOLON :: rest -> Declaration(List.rev acc), rest
    | ID name :: rest -> parse_declaration ((Id name) :: acc) rest
    | _ -> raise Invalid_statement

and parse_assignment name tokens =
  let expr_tokens, rest = split_by_token SEMICOLON tokens in
  let expr = parse_expr_raise_if_rest expr_tokens in Assignment(Id name, expr), rest

and parse_while tokens =
  let condition_tokens, rest = split_by_token DO tokens in
  let cond = parse_expr_raise_if_rest condition_tokens in
  let program, rest = parse_to_program [] rest in
  While(cond, program), rest

and parse_ite tokens = 
    let condition_tokens, then_tokens = split_by_token THEN tokens in
    let condition = parse_expr_raise_if_rest condition_tokens in
    let then_program, rest = parse_to_program [] then_tokens in
    match rest with
    | FI :: rest -> Ite(condition, then_program, []), rest
    | ELSE :: rest ->
        (let else_program, rest = parse_to_program [] rest in
        match rest with
        | FI :: rest -> Ite(condition, then_program, else_program), rest
        | _ -> raise Invalid_statement)
    | _ -> raise Invalid_statement

let parse_to_program tokens =
    match parse_to_program [] tokens with
    | result, [] -> result
    | _ -> raise Invalid_statement
