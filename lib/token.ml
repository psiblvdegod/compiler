exception Invalid_token

type token =
| ID of string
| INT of int
| WHILE
| FOR
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
