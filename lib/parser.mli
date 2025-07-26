open Types
open Token

val parse_expression : token list -> (expression, parser_error) result

;;

val parse_to_program : token list -> (program, parser_error) result

;;
