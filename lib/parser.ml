(* psiblvdegod, 2025, under MIT License *)

open Types

let rec split_by_token token left = function
  | [] -> (List.rev left, [])
  | x :: right when x = token -> (List.rev left, right)
  | x :: right -> split_by_token token (x :: left) right

let split_by_token token token_list = split_by_token token [] token_list

let match_binop_lvl_4 = function
  | EQ -> Some Eq
  | NEQ -> Some Neq
  | LEQ -> Some Leq
  | GEQ -> Some Geq
  | LT -> Some Lt
  | GT -> Some Gt
  | _ -> None

and match_binop_lvl_3 = function
  | PLUS -> Some Add
  | MINUS -> Some Sub
  | CARET -> Some Cat
  | AND -> Some And
  | OR -> Some Or
  | _ -> None

and match_binop_lvl_2 = function
  | SLASH -> Some Div
  | STAR -> Some Mul
  | _ -> None

let rec make_binop_parsing_priority_level binop_matcher next_level_parser tokens
    =
  match next_level_parser tokens with
  | Error err -> Error err
  | Ok (left, tokens) ->
      let rec parse_binop left = function
        | [] -> Ok (left, [])
        | token :: rest -> (
            match binop_matcher token with
            | None -> Ok (left, token :: rest)
            | Some binop -> (
                match next_level_parser rest with
                | Error err -> Error err
                | Ok (right, rest) ->
                    parse_binop (BinOp (binop, left, right)) rest))
      in
      parse_binop left tokens

and parse_expr_lvl_4 tokens =
  make_binop_parsing_priority_level match_binop_lvl_4 parse_expr_lvl_3 tokens

and parse_expr_lvl_3 tokens =
  make_binop_parsing_priority_level match_binop_lvl_3 parse_expr_lvl_2 tokens

and parse_expr_lvl_2 tokens =
  make_binop_parsing_priority_level match_binop_lvl_2 parse_expr_lvl_1 tokens

(* TODO: lambdas *)
and parse_expr_lvl_1 = function
  | INT n :: rest -> Ok (Int n, rest)
  | STR s :: rest -> Ok (Str s, rest)
  | ID var :: rest -> Ok (Var var, rest)
  | TRUE :: rest -> Ok (Bool true, rest)
  | FALSE :: rest -> Ok (Bool false, rest)
  | LP :: rest -> (
      match parse_expr_lvl_4 rest with
      | Error err -> Error err
      | Ok (expr, rest) -> (
          match rest with
          | RP :: rest -> Ok (expr, rest)
          | _ -> Error Invalid_expression))
  (* TODO : generalize *)
  | MINUS :: rest -> (
      match parse_expr_lvl_1 rest with
      | Error err -> Error err
      | Ok (expr, rest) -> Ok (UnOp (Neg, expr), rest))
  | BANG :: rest -> (
      match parse_expr_lvl_1 rest with
      | Error err -> Error err
      | Ok (expr, rest) -> Ok (UnOp (Not, expr), rest))
  | TILDE :: rest -> (
      match parse_expr_lvl_1 rest with
      | Error err -> Error err
      | Ok (expr, rest) -> Ok (UnOp (Rev, expr), rest))
  | _ -> Error Invalid_expression

let parse_expression tokens = parse_expr_lvl_4 tokens

let rec parse_to_program acc = function
  | [] -> Ok (List.rev acc, [])
  | VAR :: rest -> (
      match parse_declaration [] rest with
      | Error err -> Error err
      | Ok (declaration, rest) -> parse_to_program (declaration :: acc) rest)
  | WHILE :: rest -> (
      match parse_while rest with
      | Error err -> Error err
      | Ok (while_stmnt, rest) -> parse_to_program (while_stmnt :: acc) rest)
  | ID name :: COLONEQQ :: rest -> (
      match parse_assignment name rest with
      | Error err -> Error err
      | Ok (assignment, rest) -> parse_to_program (assignment :: acc) rest)
  | ID name :: rest -> (
      match parse_call name rest [] with
      | Error err -> Error err
      | Ok (call, rest) -> parse_to_program (call :: acc) rest)
  | IF :: rest -> (
      match parse_ite rest with
      | Error err -> Error err
      | Ok (statement, rest) -> parse_to_program (statement :: acc) rest)
  | ELSE :: rest -> Ok (List.rev acc, ELSE :: rest)
  | FI :: rest -> Ok (List.rev acc, FI :: rest)
  | DONE :: rest -> Ok (List.rev acc, rest)
  | END :: rest -> Ok (List.rev acc, rest)
  | DEF :: ID name :: rest -> (
      match parse_define name rest with
      | Error err -> Error err
      | Ok (definition, rest) -> parse_to_program (definition :: acc) rest)
  | _ -> Error Invalid_statement

and parse_declaration acc = function
  | SEMI :: rest -> Ok (Declaration (List.rev acc), rest)
  | ID name :: rest -> parse_declaration (name :: acc) rest
  | _ -> Error Invalid_statement

and parse_assignment name tokens =
  let expression_tokens, rest = split_by_token SEMI tokens in
  match parse_expression expression_tokens with
  | Ok (expr, []) -> Ok (Assignment (name, expr), rest)
  | _ -> Error Invalid_expression

and parse_while tokens =
  let condition_tokens, rest = split_by_token DO tokens in
  match parse_expression condition_tokens with
  | Ok (condition, []) -> (
      match parse_to_program [] rest with
      | Ok (program, rest) -> Ok (While (condition, program), rest)
      | Error err -> Error err)
  | _ -> Error Invalid_expression

and parse_ite tokens =
  let condition_tokens, then_tokens = split_by_token THEN tokens in
  match parse_expression condition_tokens with
  | Ok (condition, []) -> (
      match parse_to_program [] then_tokens with
      | Error err -> Error err
      | Ok (then_program, rest) -> (
          match rest with
          | FI :: rest -> Ok (Ite (condition, then_program, []), rest)
          | ELSE :: rest -> (
              match parse_to_program [] rest with
              | Error err -> Error err
              | Ok (else_program, rest) -> (
                  match rest with
                  | FI :: rest ->
                      Ok (Ite (condition, then_program, else_program), rest)
                  | _ -> Error Invalid_statement))
          | _ -> Error Invalid_statement))
  | _ -> Error Invalid_expression

and parse_call name tokens acc =
  match tokens with
  | [] -> Error Invalid_statement
  | SEMI :: rest -> Ok (Call (name, List.rev acc), rest)
  | tokens -> (
      match parse_expr_lvl_1 tokens with
      | Error _ -> Ok (Call (name, List.rev acc), tokens)
      | Ok (expr, rest) -> parse_call name rest (expr :: acc))

and parse_define name tokens =
  let args_tokens, rest = split_by_token IMPLIES tokens in
  let rec get_args tokens acc =
    match tokens with
    | [] -> Ok (List.rev acc)
    | LP :: ID arg_type :: ID arg :: RP :: rest ->
        get_args rest ((arg_type, arg) :: acc)
    | _ -> Error Invalid_statement
  in
  match get_args args_tokens [] with
  | Error err -> Error err
  | Ok args -> (
      match parse_to_program [] rest with
      | Error err -> Error err
      | Ok (program, rest) -> Ok (Definition (name, args, program), rest))

let parse_expression tokens =
  match parse_expression tokens with
  | Error err -> Error err
  | Ok (expression, []) -> Ok expression
  | _ -> Error Invalid_expression

let parse_to_program tokens =
  match parse_to_program [] tokens with
  | Error err -> Error err
  | Ok (program, []) -> Ok program
  | _ -> Error Invalid_statement
