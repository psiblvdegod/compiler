exception Invalid_statement

exception Invalid_expression

type expression =
  | Int of int
  | Var of string
  | Neg of expression
  | Add of expression * expression
  | Div of expression * expression
  | Mul of expression * expression
  | Sub of expression * expression
  | Call of string * expression list

and program = statement list

and statement =
  | Declaration of string list
  | Assignment of string * expression
  | While of condition * program
  | Ite of condition * program * program
  | Call of string * expression list

and condition =
  | Eq of expression * expression
  | Neq of expression * expression
  | Lt of expression * expression
  | Gt of expression * expression
  | Leq of expression * expression
  | Geq of expression * expression
