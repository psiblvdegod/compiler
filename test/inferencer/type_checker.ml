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
var b;
var c;
"

let%expect_test "test1" =
  type_check test1;
  [%expect {|
    [((Typed_Declaration [(Id "a")]), { vars = []; funcs = [] });
      ((Typed_Declaration [(Id "b")]), { vars = [((Id "a"), TNull)]; funcs = [] });
      ((Typed_Declaration [(Id "c")]),
       { vars = [((Id "a"), TNull); ((Id "b"), TNull)]; funcs = [] })
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
    [((Typed_Declaration [(Id "a"); (Id "b"); (Id "c")]),
      { vars = []; funcs = [] });
      ((Typed_Assignment ((Id "a"), (Type_Str (Typed_value "zxc")))),
       { vars = [((Id "a"), TNull); ((Id "b"), TNull); ((Id "c"), TNull)];
         funcs = [] });
      ((Typed_Assignment ((Id "b"), (Type_Str (Typed_var (Id "a"))))),
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
    [((Typed_Declaration [(Id "a"); (Id "b"); (Id "c"); (Id "d"); (Id "e")]),
      { vars = []; funcs = [] });
      ((Typed_Assignment ((Id "a"), (Type_Str (Typed_value "zxc")))),
       { vars =
         [((Id "a"), TNull); ((Id "b"), TNull); ((Id "c"), TNull);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Typed_Assignment ((Id "b"),
          (Type_Str
             (Typed_binop (Cat, (Type_Str (Typed_var (Id "a"))),
                (Type_Str (Typed_value "123")))))
          )),
       { vars =
         [((Id "a"), TStr); ((Id "b"), TNull); ((Id "c"), TNull);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Typed_Assignment ((Id "c"),
          (Type_Str
             (Typed_binop (Cat, (Type_Str (Typed_value "1")),
                (Type_Str (Typed_var (Id "b"))))))
          )),
       { vars =
         [((Id "a"), TStr); ((Id "b"), TStr); ((Id "c"), TNull);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Typed_Assignment ((Id "d"), (Type_Bool (Typed_value true)))),
       { vars =
         [((Id "b"), TStr); ((Id "a"), TStr); ((Id "c"), TStr);
           ((Id "d"), TNull); ((Id "e"), TNull)];
         funcs = [] });
      ((Typed_Assignment ((Id "e"),
          (Type_Bool
             (Typed_binop (Or, (Type_Bool (Typed_var (Id "d"))),
                (Type_Bool (Typed_value false)))))
          )),
       { vars =
         [((Id "c"), TStr); ((Id "a"), TStr); ((Id "b"), TStr);
           ((Id "d"), TBool); ((Id "e"), TNull)];
         funcs = [] })
      ]
    |}];

;;


let while_test =
"
var a;
a := true;

while a do
  
  var b;
  b := 1;

  var d;

done

var c;
"

let%expect_test "while_test" =
  type_check while_test;
  [%expect {|
    [((Typed_Declaration [(Id "a")]), { vars = []; funcs = [] });
      ((Typed_Assignment ((Id "a"), (Type_Bool (Typed_value true)))),
       { vars = [((Id "a"), TNull)]; funcs = [] });
      ((Typed_While ((Type_Bool (Typed_var (Id "a"))),
          [((Typed_Declaration [(Id "b")]),
            { vars = [((Id "a"), TBool)]; funcs = [] });
            ((Typed_Assignment ((Id "b"), (Type_Int (Typed_value 1)))),
             { vars = [((Id "a"), TBool); ((Id "b"), TNull)]; funcs = [] });
            ((Typed_Declaration [(Id "d")]),
             { vars = [((Id "a"), TBool); ((Id "b"), TInt)]; funcs = [] })
            ]
          )),
       { vars = [((Id "a"), TBool)]; funcs = [] });
      ((Typed_Declaration [(Id "c")]), { vars = [((Id "a"), TBool)]; funcs = [] })
      ]
    |}];

;;

let ite_test =
"
var a;

if true then
  var b;
  a := 1;
else
  var c;
  a := 0;
fi

var d;
"

let%expect_test "ite_test" =
  type_check ite_test;
  [%expect {|
    [((Typed_Declaration [(Id "a")]), { vars = []; funcs = [] });
      ((Typed_Ite ((Type_Bool (Typed_value true)),
          [((Typed_Declaration [(Id "b")]),
            { vars = [((Id "a"), TNull)]; funcs = [] });
            ((Typed_Assignment ((Id "a"), (Type_Int (Typed_value 1)))),
             { vars = [((Id "a"), TNull); ((Id "b"), TNull)]; funcs = [] })
            ],
          [((Typed_Declaration [(Id "c")]),
            { vars = [((Id "a"), TNull)]; funcs = [] });
            ((Typed_Assignment ((Id "a"), (Type_Int (Typed_value 0)))),
             { vars = [((Id "a"), TNull); ((Id "c"), TNull)]; funcs = [] })
            ]
          )),
       { vars = [((Id "a"), TNull)]; funcs = [] });
      ((Typed_Declaration [(Id "d")]), { vars = [((Id "a"), TNull)]; funcs = [] })
      ]
    |}];

;;
