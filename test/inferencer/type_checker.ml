open Compiler.Types
open Compiler.Lexer
open Compiler.Parser
open Compiler.Inferencer

let type_check str = 
  match str |> tokenize with
  | Error err -> print_endline ("Error: " ^ (show_lexer_error err))
  | Ok(tokens) ->
    match tokens |> parse_to_program with
    | Error err -> print_endline ("Error: " ^ (show_parser_error err))
    | Ok(program) ->
      match program |> infer_types with
      | Error err -> print_endline ("Error: " ^ (show_inferencer_error err))
      | Ok(inferencer_state) -> print_endline (show_inferencer_state inferencer_state)

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

let%expect_test "test1" =
  type_check test1;
  [%expect {|
    { vars =
      [((Id "f"), TBool); ((Id "e"), TBool); ((Id "d"), TInt); ((Id "c"), TInt);
        ((Id "b"), TInt); ((Id "a"), TInt)];
      funcs = [] }
    |}];

;;

let test2 =
"
var a b c;

a := \"zxc\";

b := a;

"

let%expect_test "test2" =
  type_check test2;
  [%expect {|
    { vars = [((Id "b"), TStr); ((Id "a"), TStr); ((Id "c"), TNull)]; funcs = []
      }
    |}];

;;


let test3 =
"
var a b c d e;

a := \"zxc\";

b := a ^ \"123\";

c := \"1\" ^ b;

d := true;

e := d or false;

"

let%expect_test "test3" =
  type_check test3;
  [%expect {|
    { vars =
      [((Id "e"), TBool); ((Id "d"), TBool); ((Id "c"), TStr); ((Id "b"), TStr);
        ((Id "a"), TStr)];
      funcs = [] }
    |}];

;;
