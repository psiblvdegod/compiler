open Types
open Token

val parse_expression : token list -> (expression, Types.error) result

;;

val parse_to_program : token list -> (program, Types.error) result

;;
