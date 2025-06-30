type expression =
  | Integer of int
  | Variable of string

  | Assignment of string * expression

  | Addition of expression * expression

  | Multiplication of expression * expression
