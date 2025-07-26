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
      | Ok(typed_program) -> print_endline (show_typed_program typed_program)
  
let test1 =
"
var a;
a := 1;
var b;
b := a;
"

let%expect_test "test1" =
  type_check test1;
  [%expect {|
    [((Declaration [(Id "a")]), { vars = []; funcs = [] });
      ((Assignment ((Id "a"), (Int 1))),
       { vars = [((Id "a"), TNull)]; funcs = [] });
      ((Declaration [(Id "b")]), { vars = [((Id "a"), TInt)]; funcs = [] });
      ((Assignment ((Id "b"), (Var (Id "a")))),
       { vars = [((Id "a"), TInt); ((Id "b"), TNull)]; funcs = [] })
      ]
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
    [((Declaration [(Id "a"); (Id "b"); (Id "c")]), { vars = []; funcs = [] });
      ((Assignment ((Id "a"), (Str "zxc"))),
       { vars = [((Id "a"), TNull); ((Id "b"), TNull); ((Id "c"), TNull)];
         funcs = [] });
      ((Assignment ((Id "b"), (Var (Id "a")))),
       { vars = [((Id "a"), TStr); ((Id "b"), TNull); ((Id "c"), TNull)];
         funcs = [] })
      ]
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
    [((Declaration [(Id "a"); (Id "b"); (Id "c"); (Id "d"); (Id "e")]),
      { vars = []; funcs = [] });
      ((Assignment ((Id "a"), (Str "zxc"))),
       { vars =
         [((Id "a"), TNull); ((Id "b"), TNull); ((Id "c"), TNull);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Assignment ((Id "b"), (BinOp (Cat, (Var (Id "a")), (Str "123"))))),
       { vars =
         [((Id "a"), TStr); ((Id "b"), TNull); ((Id "c"), TNull);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Assignment ((Id "c"), (BinOp (Cat, (Str "1"), (Var (Id "b")))))),
       { vars =
         [((Id "b"), TStr); ((Id "a"), TStr); ((Id "c"), TNull);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Assignment ((Id "d"), (Bool true))),
       { vars =
         [((Id "c"), TStr); ((Id "b"), TStr); ((Id "a"), TStr);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Assignment ((Id "e"), (BinOp (Or, (Var (Id "d")), (Bool false))))),
       { vars =
         [((Id "d"), TBool); ((Id "c"), TStr); ((Id "b"), TStr);
           ((Id "a"), TStr); ((Id "e"), TNull)];
         funcs = [] })
      ]
    |}];

;;
