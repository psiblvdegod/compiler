open Lexer.Token
open Types
open Parse_expression

let rec parse_condition left = function
  | [] -> raise Invalid_expression
  | EQ :: right -> Eq(List.rev left |> parse_expression, parse_expression right)
  | NEQ :: right -> Neq(List.rev left |> parse_expression, parse_expression right)
  | LEQ :: right -> Leq(List.rev left |> parse_expression, parse_expression right)
  | GEQ :: right -> Geq(List.rev left |> parse_expression, parse_expression right)
  | x :: right -> parse_condition (x :: left) (right)

let parse_condition tokens = parse_condition [] tokens
