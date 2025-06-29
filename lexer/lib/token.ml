type token =
  | WHILE
  | FOR
  | ID of string
  | INT of int
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQ
  | NEQ
  | LP
  | RP
  | EOF

let string_of_token token =
  match token with
    | WHILE  ->  "WHILE"
    | FOR    ->  "FOR"
    | PLUS   ->  "PLUS"
    | MINUS  ->  "MINUS"
    | STAR   ->  "STAR"
    | SLASH  ->  "SLASH"
    | EQ     ->  "EQ"
    | NEQ    ->  "NEQ"
    | LP     ->  "LP"
    | RP     ->  "RP"
    | EOF    ->  "EOF"
    | ID s   ->  s
    | INT n  ->  string_of_int n

exception Invalid_expression of string
