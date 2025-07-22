exception Invalid_statement
exception Invalid_expression

type id = Id of string

type expression =
  | Var of id

  | Int of int
  | Bool of bool
  | Str of string

  | BinOp of binary_operation * expression * expression
  | UnOp of unary_operation * expression
  | Call of call

and unary_operation =
  | Neg
  | Rev
  | Not

and binary_operation =
  | Mul
  | Div
  
  | Add
  | Sub
  | Cat
  | And
  | Or

  | Eq
  | Neq
  | Leq
  | Geq
  | Lt
  | Gt

and call = id * expression list

and statement =
  | Declaration of id list

  | Assignment of id * expression
  | While of expression * program
  | Ite of expression * program * program

  | Definition of id * id list * program
  | Call of call

and program = statement list
