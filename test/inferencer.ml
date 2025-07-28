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
    [((Typed_Declaration ["a"]), { vars = []; funcs = [] });
      ((Typed_Declaration ["b"]), { vars = [("a", TNull)]; funcs = [] });
      ((Typed_Declaration ["c"]),
       { vars = [("a", TNull); ("b", TNull)]; funcs = [] })
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
    [((Typed_Declaration ["a"; "b"; "c"]), { vars = []; funcs = [] });
      ((Typed_Assignment ("a", (Type_Str (Typed_value "zxc")))),
       { vars = [("a", TNull); ("b", TNull); ("c", TNull)]; funcs = [] });
      ((Typed_Assignment ("b", (Type_Str (Typed_var "a")))),
       { vars = [("a", TStr); ("b", TNull); ("c", TNull)]; funcs = [] })
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
    [((Typed_Declaration ["a"; "b"; "c"; "d"; "e"]), { vars = []; funcs = [] });
      ((Typed_Assignment ("a", (Type_Str (Typed_value "zxc")))),
       { vars =
         [("a", TNull); ("b", TNull); ("c", TNull); ("d", TNull); ("e", TNull)];
         funcs = [] });
      ((Typed_Assignment ("b",
          (Type_Str
             (Typed_binop (Cat, (Type_Str (Typed_var "a")),
                (Type_Str (Typed_value "123")))))
          )),
       { vars =
         [("a", TStr); ("b", TNull); ("c", TNull); ("d", TNull); ("e", TNull)];
         funcs = [] });
      ((Typed_Assignment ("c",
          (Type_Str
             (Typed_binop (Cat, (Type_Str (Typed_value "1")),
                (Type_Str (Typed_var "b")))))
          )),
       { vars =
         [("a", TStr); ("b", TStr); ("c", TNull); ("d", TNull); ("e", TNull)];
         funcs = [] });
      ((Typed_Assignment ("d", (Type_Bool (Typed_value true)))),
       { vars =
         [("a", TStr); ("b", TStr); ("c", TStr); ("d", TNull); ("e", TNull)];
         funcs = [] });
      ((Typed_Assignment ("e",
          (Type_Bool
             (Typed_binop (Or, (Type_Bool (Typed_var "d")),
                (Type_Bool (Typed_value false)))))
          )),
       { vars =
         [("a", TStr); ("b", TStr); ("c", TStr); ("d", TBool); ("e", TNull)];
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
    [((Typed_Declaration ["a"]), { vars = []; funcs = [] });
      ((Typed_Assignment ("a", (Type_Bool (Typed_value true)))),
       { vars = [("a", TNull)]; funcs = [] });
      ((Typed_While ((Type_Bool (Typed_var "a")),
          [((Typed_Declaration ["b"]), { vars = [("a", TBool)]; funcs = [] });
            ((Typed_Assignment ("b", (Type_Int (Typed_value 1)))),
             { vars = [("a", TBool); ("b", TNull)]; funcs = [] });
            ((Typed_Declaration ["d"]),
             { vars = [("a", TBool); ("b", TInt)]; funcs = [] })
            ]
          )),
       { vars = [("a", TBool)]; funcs = [] });
      ((Typed_Declaration ["c"]), { vars = [("a", TBool)]; funcs = [] })]
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
    [((Typed_Declaration ["a"]), { vars = []; funcs = [] });
      ((Typed_Ite ((Type_Bool (Typed_value true)),
          [((Typed_Declaration ["b"]), { vars = [("a", TNull)]; funcs = [] });
            ((Typed_Assignment ("a", (Type_Int (Typed_value 1)))),
             { vars = [("a", TNull); ("b", TNull)]; funcs = [] })
            ],
          [((Typed_Declaration ["c"]), { vars = [("a", TNull)]; funcs = [] });
            ((Typed_Assignment ("a", (Type_Int (Typed_value 0)))),
             { vars = [("a", TNull); ("c", TNull)]; funcs = [] })
            ]
          )),
       { vars = [("a", TNull)]; funcs = [] });
      ((Typed_Declaration ["d"]), { vars = [("a", TNull)]; funcs = [] })]
    |}];

;;
