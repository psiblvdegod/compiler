exception Invalid_statement
exception Invalid_expression

type id = Id of string

type expression =
  | Var of id

  | Int of int
  | Bool of bool
  | Str of string

  | BinOp of binary_operation
  | UnOp of unary_operation

and unary_operation =
  | Neg of expression (* x -> -x *)
  | Rev of expression 
  | Not of expression 

and binary_operation =
  | Mul of expression * expression
  | Div of expression * expression
  
  | Add of expression * expression
  | Sub of expression * expression
  | Cat of expression * expression
  | And of expression * expression
  | Or  of expression * expression

  | Eq  of expression * expression
  | Neq of expression * expression
  | Leq of expression * expression
  | Geq of expression * expression
  | Lt  of expression * expression
  | Gt  of expression * expression

and statement =
  | Declaration of id list

  | Assignment of id * expression
  | While of expression * program
  | Ite of expression * program * program

  | Definition of id * id list * program
  | Call of id * expression list

and program = statement list
