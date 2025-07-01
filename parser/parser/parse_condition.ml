open Lexer.Token
open Types
open Parse_expression

let rec parse_condition left = function
  | [] -> raise Invalid_expression
  | EQ :: right -> Eq(parse_expression (List.rev left), parse_expression right)
  | NEQ :: right -> Neq(parse_expression (List.rev left), parse_expression right)
  | LEQ :: right -> Leq(parse_expression (List.rev left), parse_expression right)
  | GEQ :: right -> Geq(parse_expression (List.rev left), parse_expression right)
  | x :: right -> parse_condition (x :: left) (right)

let parse_condition tokens = parse_condition [] tokens
