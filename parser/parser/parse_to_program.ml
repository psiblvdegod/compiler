open Parse_expression
open Parse_condition
open Lexer.Processing
open Lexer.Token
open Types

let rec split_by_token token left = function
  | [] -> List.rev left, []
  | x :: right when x = token -> List.rev left, right
  | x :: right -> split_by_token token (x :: left) right

let split_by_token token sequence = split_by_token token [] sequence

let rec parse_to_program result tokens =
  match tokens with
  | [] -> List.rev result, []
  | EOF :: _ -> List.rev result, []
  | VAR :: rest ->
    let assignment, rest = parse_assignment rest in
    parse_to_program (assignment :: result) rest
  | WHILE :: rest ->
    let condition, statements, rest = parse_while rest in
    parse_to_program (While(condition, statements) :: result) rest
  | DONE :: rest -> List.rev result, rest
  | _ -> raise Invalid_statement

and parse_while tokens =
  let (condition_tokens, statements_tokens) = split_by_token DO tokens in
  let condition = parse_condition condition_tokens in
  let (statements, rest) = parse_to_program [] statements_tokens in
  condition, statements, rest

and parse_assignment = function
  | ID name :: tokens ->
    (match tokens with
    | COLONEQQ :: tokens -> 
      let statement_tokens, rest = split_by_token SEMICOLON tokens in
        Assignment(name, parse_expression statement_tokens), rest
    | _ -> raise Invalid_statement)
  | _ -> raise Invalid_statement

let parse_to_program source =
  let (result, _) = parse_to_program [] (tokens_of_string source) in result
