exception Invalid_token

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
