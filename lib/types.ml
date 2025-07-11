exception Invalid_statement

exception Invalid_expression

type expression =
  | Int of int
  | Var of string
  | Add of expression * expression
  | Div of expression * expression
  | Mul of expression * expression
  | Sub of expression * expression
  | Call of string * expression list

and program = statement list

and statement =
  | Assignment of string * expression
  | While of comparison * program
  | If of comparison * program

and comparison =
  | Eq of expression * expression
  | Neq of expression * expression
  | Lt of expression * expression
  | Gt of expression * expression
  | Leq of expression * expression
  | Geq of expression * expression
