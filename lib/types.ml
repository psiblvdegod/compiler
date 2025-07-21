exception Invalid_statement
exception Invalid_expression

type expr =
  | Int_Expr of int_expr
  | Bool_Expr of bool_expr
  | Str_Expr of str_expr

and int_expr =
  | Int of int
  | Var of string
  | Neg of int_expr
  | BinOp of int_binop * int_expr * int_expr

and int_binop =
  | Add
  | Sub
  | Div
  | Mul

and bool_expr =
  | True
  | False
  | Var of string
  | IntCmp of int_cmp * int_expr * int_expr
  | StrCmp of str_expr * str_expr

and int_cmp =
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq

and str_expr =
  | Str of string
  | Var of string
  | StrOp of str_op

and str_op =
  | Rev of str_expr
  | Cat of str_expr list

and statement =
  | Declaration of string list
  | Definition of string * string list * program
  | Assignment of string * expr
  | While of bool_expr * program
  | Ite of bool_expr * program * program
  | Call of string * expr list

and program = statement list
