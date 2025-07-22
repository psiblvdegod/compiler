open Token
open Types

let rec split_by_token token left = function
  | [] -> List.rev left, []
  | x :: right when x = token -> List.rev left, right
  | x :: right -> split_by_token token (x :: left) right

let split_by_token token token_list = split_by_token token [] token_list

let match_binop_lvl_4 = function
    | EQ  -> Some Eq
    | NEQ -> Some Neq
    | LEQ -> Some Leq
    | GEQ -> Some Geq
    | LT  -> Some Lt
    | GT  -> Some Gt
    | _   -> None

and match_binop_lvl_3 = function
    | PLUS  -> Some Add
    | MINUS -> Some Sub
    | _     -> None

and match_binop_lvl_2 = function
    | SLASH -> Some Div    
    | STAR -> Some Mul
    | _     -> None

(* TODO: fix? god object *)
let rec make_binop_parsing_priority_level binop_matcher next_level_parser tokens =
    let (left, tokens) = next_level_parser tokens in
    let rec parse_binop left = function
    | [] -> (left, [])
    | token :: rest ->
        match binop_matcher token with
        | None -> (left, token :: rest)
        | Some binop ->
            let (right, rest) = next_level_parser rest in
            parse_binop (BinOp(binop, left, right)) rest in

    parse_binop left tokens

and parse_expr_lvl_4 tokens = make_binop_parsing_priority_level match_binop_lvl_4 parse_expr_lvl_3 tokens

and parse_expr_lvl_3 tokens = make_binop_parsing_priority_level match_binop_lvl_3 parse_expr_lvl_2 tokens

and parse_expr_lvl_2 tokens = make_binop_parsing_priority_level match_binop_lvl_2 parse_expr_lvl_1 tokens

and parse_expr_lvl_1 = function
    | [] -> raise Invalid_expression

    | INT n :: rest -> Int n, rest
    | ID id :: rest -> Var (Id id), rest (* TODO : parse call *)
    | TRUE  :: rest -> Bool true, rest
    | FALSE :: rest -> Bool false, rest
    | MINUS :: rest -> let (expr, rest) = parse_expr_lvl_1 rest in UnOp(Neg, expr), rest
    | LP    :: rest ->
        let (expr, rest) = parse_expr_lvl_4 rest in
        (match rest with
        | RP :: rest -> expr, rest
        | _ -> raise Invalid_expression)
    | _ -> raise Invalid_expression

let parse_expr_with_rest tokens = parse_expr_lvl_4 tokens

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
