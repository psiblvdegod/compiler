(* psiblvdegod, 2025, under MIT License *)

open Compiler.Types
open Compiler.Lexer
open Compiler.Parser
open Compiler.Inferencer

let pp_annotated_ast str =
  match str |> tokenize with
  | Error err -> print_endline ("Error: " ^ show_lexer_error err)
  | Ok tokens -> (
      match tokens |> parse_to_program with
      | Error err -> print_endline ("Error: " ^ show_parser_error err)
      | Ok program -> (
          match program |> infer_types with
          | Error err -> print_endline ("Error: " ^ show_inferencer_error err)
          | Ok typed_program -> print_endline (show_typed_program typed_program)
          ))

let _declaration_test = "
var a;
var b;
var c;
"

let%expect_test "declaration_test" =
  pp_annotated_ast _declaration_test;
  [%expect
    {|
    [((Typed_Declaration ["a"]), { vars = []; funcs = [] });
      ((Typed_Declaration ["b"]), { vars = [("a", TNull)]; funcs = [] });
      ((Typed_Declaration ["c"]),
       { vars = [("a", TNull); ("b", TNull)]; funcs = [] })
      ]
    |}]

let _assignment_test = "
var a b c;

a := \"zxc\";

b := a;

"

let%expect_test "assignment_test" =
  pp_annotated_ast _assignment_test;
  [%expect
    {|
    [((Typed_Declaration ["a"; "b"; "c"]), { vars = []; funcs = [] });
      ((Typed_Assignment ("a", (Type_Str (Typed_value "zxc")))),
       { vars = [("a", TNull); ("b", TNull); ("c", TNull)]; funcs = [] });
      ((Typed_Assignment ("b", (Type_Str (Typed_var "a")))),
       { vars = [("a", TStr); ("b", TNull); ("c", TNull)]; funcs = [] })
      ]
    |}]

let _operations_test =
  "
var a b c d e;

a := \"zxc\";

b := a ^ \"123\";

c := \"1\" ^ b;

d := true;

e := d or false;
"

let%expect_test "operations_test" =
  pp_annotated_ast _operations_test;
  [%expect
    {|
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
    |}]

let _while_test =
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
  pp_annotated_ast _while_test;
  [%expect
    {|
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
    |}]

let _ite_test =
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
  pp_annotated_ast _ite_test;
  [%expect
    {|
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
    |}]

let _definition_printsum =
  "
def printsum (int left) (int right) =>
  print (left + right);
end
"

let%expect_test "definition_printsum" =
  pp_annotated_ast _definition_printsum;
  [%expect
    {|
    [((Typed_Definition ("printsum", [("right", TInt); ("left", TInt)],
         [((Typed_Call ("print",
              [(Type_Int
                  (Typed_binop (Add, (Type_Int (Typed_var "left")),
                     (Type_Int (Typed_var "right")))))
                ]
              )),
           { vars = [("right", TInt); ("left", TInt)];
             funcs = [("printsum", [("right", TInt); ("left", TInt)])] })
           ]
         )),
      { vars = [("right", TInt); ("left", TInt)];
        funcs = [("printsum", [("right", TInt); ("left", TInt)])] })
      ]
    |}]

let _definition_factorial =
  "
def fact (int n) (int acc) =>
  if n == 0 then
      print(acc);
  else
      fact (n - 1) (n * acc);
  fi
end
"

let%expect_test "definition_factorial" =
  pp_annotated_ast _definition_factorial;
  [%expect
    {|
    [((Typed_Definition ("fact", [("acc", TInt); ("n", TInt)],
         [((Typed_Ite (
              (Type_Bool
                 (Typed_binop (Eq, (Type_Int (Typed_var "n")),
                    (Type_Int (Typed_value 0))))),
              [((Typed_Call ("print", [(Type_Int (Typed_var "acc"))])),
                { vars = [("acc", TInt); ("n", TInt)];
                  funcs = [("fact", [("acc", TInt); ("n", TInt)])] })
                ],
              [((Typed_Call ("fact",
                   [(Type_Int
                       (Typed_binop (Sub, (Type_Int (Typed_var "n")),
                          (Type_Int (Typed_value 1)))));
                     (Type_Int
                        (Typed_binop (Mul, (Type_Int (Typed_var "n")),
                           (Type_Int (Typed_var "acc")))))
                     ]
                   )),
                { vars = [("acc", TInt); ("n", TInt)];
                  funcs = [("fact", [("acc", TInt); ("n", TInt)])] })
                ]
              )),
           { vars = [("acc", TInt); ("n", TInt)];
             funcs = [("fact", [("acc", TInt); ("n", TInt)])] })
           ]
         )),
      { vars = [("acc", TInt); ("n", TInt)];
        funcs = [("fact", [("acc", TInt); ("n", TInt)])] })
      ]
    |}]

let _definition =
  "
def helloworld =>
  print \"hello world!!!\";
end

helloworld;
"

let%expect_test "_definition" =
  pp_annotated_ast _definition;
  [%expect
    {|
    [((Typed_Definition ("helloworld", [],
         [((Typed_Call ("print", [(Type_Str (Typed_value "hello world!!!"))])),
           { vars = []; funcs = [("helloworld", [])] })]
         )),
      { vars = []; funcs = [("helloworld", [])] });
      ((Typed_Call ("helloworld", [])),
       { vars = []; funcs = [("helloworld", [])] })
      ]
    |}]
