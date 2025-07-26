type error =
  | Invalid_expression
  | Invalid_statement
[@@deriving show { with_path = false }]

type expression =
  | Var of id

  | Int of int
  | Bool of bool
  | Str of string

  | BinOp of binary_operation * expression * expression
  | UnOp of unary_operation * expression
[@@deriving show { with_path = false }]

and id = Id of string
[@@deriving show { with_path = false }]

and unary_operation =
  | Neg
  | Rev
  | Not
[@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

and call = id * expression list
[@@deriving show { with_path = false }]

and statement =
  | Declaration of id list

  | Assignment of id * expression
  | While of expression * program
  | Ite of expression * program * program

  | Definition of id * id list * program
  | Call of call
[@@deriving show { with_path = false }]

and program = statement list
[@@deriving show { with_path = false }]

(* inferencer *)

type inferencer_error =
    | Already_declared
    | Was_Not_declared
    | Was_Not_defined
    | Was_Not_assigned
    | Operand_type_dismatch
    | Function_type_dismatch
    | Already_specified
[@@deriving show { with_path = false }]

type expression_type = | TInt | TBool | TStr | TNull
[@@deriving show { with_path = false }]

and inferencer_state = 
{
    vars : (id * expression_type) list;
    funcs: (id * expression_type * (expression_type list)) list;
}
[@@deriving show { with_path = false }]
