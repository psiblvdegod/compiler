open Lexer.Token
open Expression
exception Parse_error

let rec parse_to_ast tokens =
    match lowpr_outer tokens with
    | result, [EOF] -> result
    | _ -> raise Parse_error

and lowpr_outer tokens =
    let (left, rest) = highpr_outer tokens in
    lowpr_inner left rest

and lowpr_inner left = function
    | PLUS :: rest ->
        let (right, rest) = highpr_outer rest in
        lowpr_inner (Addition(left, right)) rest
    | other -> (left, other)

and highpr_outer tokens = 
    let (left, rest) = parse_term tokens in
    highpr_inner left rest

and highpr_inner left = function
    | STAR :: rest ->
        let (right, rest) = parse_term rest in
        highpr_inner (Multiplication(left, right)) rest
    | other -> (left, other)

and parse_term = function
    | [] -> raise Parse_error
    | ID var :: rest -> Variable var, rest
    | INT n :: rest -> Integer n, rest
    | _ -> raise Parse_error
