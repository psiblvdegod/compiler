{
  exception Invalid_token of string

  type token =
  | WHILE
  | FOR
  | DO
  | DONE

  | SEMICOLON
  | ASSIGN
  | VAR

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
}

rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf }
  | ['0'-'9']+ as n       { INT (int_of_string n) }

  | "while"   { WHILE }
  | "for"     { FOR }
  | "done"    { DONE }
  | "do"      { DO }

  | ":="      { ASSIGN }
  | "var"     { VAR }
  | ";"       { SEMICOLON }

  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { STAR }
  | '/'       { SLASH }

  | "!="      { NEQ }
  | "=="      { EQ }

  | '('       { LP }
  | ')'       { RP }
  
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }
  
  | eof   { EOF }
  | _     { raise (Invalid_token "invalid symbol") }
