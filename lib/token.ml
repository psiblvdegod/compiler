type error =
| Invalid_token
| Input_is_empty
[@@deriving show { with_path = false }]

type token =
| ID of string
| INT of int
| TRUE
| FALSE
| STR of string
| WHILE
| DO
| DONE
| VAR
| IF
| THEN
| ELSE
| FI
| PLUS
| MINUS
| STAR
| SLASH
| CAT
| AND
| OR
| EQ
| NEQ
| COLONEQQ
| LEQ
| GEQ
| LT
| GT
| LP
| RP
| SEMICOLON
[@@deriving show { with_path = false }]
