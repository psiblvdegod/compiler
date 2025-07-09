exception Invalid_token

type token =
| ID of string
| INT of int

| WHILE
| FOR
| DO
| DONE

| SEMICOLON
| COLONEQQ
| VAR

| PLUS
| MINUS
| STAR
| SLASH

| EQ
| NEQ
| LEQ
| GEQ
| LT
| GT

| LP
| RP

| EOF
