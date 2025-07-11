open Token
open Types

val parse_to_program : token list -> program

val parse_comparison : token list -> comparison

val parse_expression : token list -> expression
