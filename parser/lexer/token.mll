{
  exception Invalid_token

  type token =
  | WHILE
  | FOR
  | DO
  | DONE

  | CALL

  | SEMICOLON
  | COLONEQQ
  | VAR

  | ID of string
  | INT of int

  | PLUS
  | MINUS
  | STAR
  | SLASH

  | EQ
  | NEQ
  | LEQ
  | GEQ

  | LP
  | RP
  | EOF
}

rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf }
  | ['0'-'9']+ as n       { INT (int_of_string n) }

  | "while"   { WHILE }
  | "for"     { FOR }
  | "done"    { DONE }
  | "do"      { DO }

  | "call"    { CALL }

  | ":="      { COLONEQQ }
  | "var"     { VAR }
  | ";"       { SEMICOLON }

  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { STAR }
  | '/'       { SLASH }

  | "!="      { NEQ }
  | "=="      { EQ }
  | "<="      { LEQ }
  | ">="      { GEQ }

  | '('       { LP }
  | ')'       { RP }
  
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }
  
  | eof   { EOF }
  | _     { raise Invalid_token }
