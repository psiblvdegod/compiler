open Lexer.Token
open Ast

exception Parse_error

let rec parse_main = function
    | [] -> raise Parse_error
    | tokens ->
        let (left, rest) = parse_term tokens in
        parse_expr left rest

and parse_expr left = function
    | [] -> (left, [])
    | PLUS :: rest ->
        let (right, rest) = parse_term rest in
        parse_expr (Addition(left, right)) rest
    | x -> (left, x)

and parse_term = function
    | [] -> raise Parse_error
    | ID id :: rest -> Variable id, rest
    | INT x :: rest -> Integer x, rest
    | _ -> raise Parse_error

let parse_to_ast tokens = 
    let (l,_) = parse_main tokens in l
