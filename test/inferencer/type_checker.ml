open Compiler
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

let test2 =
"
var a b c;

a := \"zxc\";

b := a;

"

let type_check str = 
  match str |> tokenize with
  | Error err -> print_endline ("Error: " ^ (Token.show_error err))
  | Ok(tokens) ->
    match tokens |> parse_to_program with
    | Error err -> print_endline ("Error: " ^ (Types.show_error err))
    | Ok(program) -> program |> pp_types_of_program
  


let%expect_test "test1" =
  type_check test1;
  [%expect {|
    Name: a | Type: Integer
    Name: b | Type: Integer
    Name: c | Type: Integer
    Name: d | Type: Integer
    Name: e | Type: Boolean
    Name: f | Type: Boolean
    |}];

;;

let%expect_test "test2" =
  type_check test2;
  [%expect {|
    Name: c | Type: Not specified
    Name: a | Type: String
    Name: b | Type: String
    |}];

;;
