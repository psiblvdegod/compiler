{
  open Token
}

rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf }
  | ['0'-'9']+ as n       { INT (int_of_string n) }
  | "while"               { WHILE }
  | "for"                 { FOR }

  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { STAR }
  | '/'   { SLASH }

  | "!="  { NEQ }
  | "=="  { EQ }

  | ":="  { ASSIGN }

  | '('   { LP }
  | ')'   { RP }
  
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }
  
  | eof   { EOF }
  | _     { raise (Invalid_expression "invalid symbol") }
