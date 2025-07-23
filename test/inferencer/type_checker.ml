open Compiler.Lexer
open Compiler.Parser
open Compiler.Inferencer

let test1 =
"
var a b c d e f;

a := 1;
b := 2;
c := a + b;
d := c / (10 - a) * b - 0;
e := b < a;
f := e;
"

let type_check str = str |> tokenize |> parse_to_program |> pp_types_of_program

let%expect_test "test1" =
  type_check test1;
  [%expect {| 5040 |}];

;;
