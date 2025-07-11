open Token
open Types

val parse_to_program : token list -> program

val parse_condition : token list -> condition

val parse_expression : token list -> expression
