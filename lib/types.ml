(* lexer *)

type lexer_error =
| Invalid_token
| Input_is_empty
[@@deriving show { with_path = false }]

type token =
| ID of string
| INT of int
| TRUE
| FALSE
| STR of string
| WHILE
| DO
| DONE
| VAR
| IF
| THEN
| ELSE
| FI
| PLUS
| MINUS
| STAR
| SLASH
| CAT
| AND
| OR
| EQ
| NEQ
| COLONEQQ
| LEQ
| GEQ
| LT
| GT
| LP
| RP
| SEMICOLON
[@@deriving show { with_path = false }]

(* parser *)

type parser_error =
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

and statement =
  | Declaration of id list

  | Assignment of id * expression
  | While of expression * program
  | Ite of expression * program * program

  | Definition of id * id list * program
  | Call of id * expression list
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
    | Expression_type_dismatch
    | Function_type_dismatch
    | Already_specified
[@@deriving show { with_path = false }]

type expression_type = | TInt | TBool | TStr | TNull
[@@deriving show { with_path = false }]

and scope = 
{
    vars : (id * expression_type) list;
    funcs: (id * expression_type * (expression_type list)) list;
}
[@@deriving show { with_path = false }]

and typed_expression = expression * expression_type

and typed_statement =
  | Typed_Declaration of id list

  | Typed_Assignment of id * typed_expression
  | Typed_While of typed_expression * typed_program
  | Typed_Ite of typed_expression * typed_program * typed_program

  | Typed_Definition of id * id list * typed_program
  | Typed_Call of id * typed_expression list
[@@deriving show { with_path = false }]

and typed_program = (typed_statement * scope) list
[@@deriving show { with_path = false }]

(* generator *)

type generator_error =
| Not_implemented
[@@deriving show { with_path = false }]
