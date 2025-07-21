open Token
open Types

val parse_to_program : token list -> program

val parse_bool_expr : token list -> bool_expr

val parse_int_expr : token list -> int_expr

val parse_str_expr : token list -> str_expr
