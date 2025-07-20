exception Invalid_statement
exception Invalid_expression

type expression =
  | Int of int
  | Var of string
  | Neg of expression
  | BinOp of binary_operation * expression * expression
  | Call of string * expression list

and binary_operation =
  | Add
  | Sub
  | Div
  | Mul

and program = statement list

and statement =
  | Declaration of string list
  | Assignment of string * expression
  | While of boolean_expression * program
  | Ite of boolean_expression * program * program
  | Call of string * expression list

and boolean_expression =
  | true
  | false
  | Comparison of comparison * expression * expression

and comparison =
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
