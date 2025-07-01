open Types
open Lexer.Token

let rec parse_expression tokens =
    let result, _ = lowpr_outer tokens in result

and lowpr_outer tokens =
    let (left, rest) = highpr_outer tokens in lowpr_inner left rest

and highpr_outer tokens = 
    let (left, rest) = parse_term tokens in highpr_inner left rest

and lowpr_inner left = function
    | PLUS :: rest ->
        let (right, rest) = highpr_outer rest in
        lowpr_inner (Add(left, right)) rest
    | MINUS :: rest ->
        let (right, rest) = highpr_outer rest in
        lowpr_inner (Sub(left, right)) rest
    | other -> (left, other)

and highpr_inner left = function
    | STAR :: rest ->
        let (right, rest) = parse_term rest in
        highpr_inner (Mul(left, right)) rest
    | SLASH :: rest ->
        let (right, rest) = parse_term rest in
        highpr_inner (Div(left, right)) rest
    | other -> (left, other)

and parse_term = function
    | [] -> raise Invalid_expression
    | ID var :: rest -> Var var, rest
    | INT n :: rest -> Int n, rest
    | LP :: rest ->
        let (expr, rest) = lowpr_outer rest in
        (match rest with
        | RP :: rest -> expr, rest
        | _ -> raise Invalid_expression)
    | _ -> raise Invalid_expression
