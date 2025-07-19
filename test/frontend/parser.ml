open Compiler
open Compiler.Types

let pp_ast text =
  match Lexer.tokenize text with
  | Error err -> print_endline ("Error: " ^ show_lexer_error err)
  | Ok(tokens) ->
  match Parser.parse_expression tokens with
  | Error err -> print_endline ("Error: " ^ show_parser_error err)
  | Ok(expression) -> print_endline (Types.show_expression expression)

let _parse_expression_correct_1 = "1 + 2 * 3"

let%expect_test "parse_expression_correct_1" =
    pp_ast _parse_expression_correct_1;
    [%expect {| (BinOp (Add, (Int 1), (BinOp (Mul, (Int 2), (Int 3))))) |}]

;;

let _parse_expression_correct_2 = "asd + zxc * 42 + qwe" 

let%expect_test "parse_expression_correct_2" =
    pp_ast _parse_expression_correct_2;
    [%expect {|
      (BinOp (Add,
         (BinOp (Add, (Var "asd"), (BinOp (Mul, (Var "zxc"), (Int 42))))),
         (Var "qwe")))
      |}]

;;

let _parse_expression_correct_3 = "a * (b + c)"

let%expect_test "parse_expression_correct_3" =
    pp_ast _parse_expression_correct_3;
    [%expect {| (BinOp (Mul, (Var "a"), (BinOp (Add, (Var "b"), (Var "c"))))) |}]

;;

let _parse_expression_correct_4 = "a - (b + c) * (1 / 2)"

let%expect_test "parse_expression_correct_4" =
    pp_ast _parse_expression_correct_4;
    [%expect {|
      (BinOp (Sub, (Var "a"),
         (BinOp (Mul, (BinOp (Add, (Var "b"), (Var "c"))),
            (BinOp (Div, (Int 1), (Int 2)))))
         ))
      |}]

;;

let _parse_expression_correct_5 = "~(str ^ str) and -b or !(true - false)"

let%expect_test "test on correct input 5" =
  pp_ast _parse_expression_correct_5;
  [%expect {|
    (BinOp (Or,
       (BinOp (And, (UnOp (Rev, (BinOp (Cat, (Var "str"), (Var "str"))))),
          (UnOp (Neg, (Var "b"))))),
       (UnOp (Not, (BinOp (Sub, (Bool true), (Bool false)))))))
    |}];

;;

let pp_program text = 
  match Lexer.tokenize text with
  | Error err -> print_endline ("Error: " ^ show_lexer_error err)
  | Ok(tokens) ->
  match Parser.parse_to_program tokens with
  | Error err -> print_endline ("Error: " ^ show_parser_error err)
  | Ok(program) -> print_endline(Types.show_program program)

let _assignment_correct_1 = "a := b; c := 5;"

let%expect_test "assignment_correct_1" =
    pp_program _assignment_correct_1;
    [%expect {| [(Assignment ("a", (Var "b"))); (Assignment ("c", (Int 5)))] |}]

;;

let _assignment_correct_2 = "a := 1 + b * 2;"

let%expect_test "assignment_correct_1" =
    pp_program _assignment_correct_2;
    [%expect {|
      [(Assignment ("a", (BinOp (Add, (Int 1), (BinOp (Mul, (Var "b"), (Int 2)))))
          ))
        ]
      |}]

;;

let _assignment_incorrect_1 = "a := while * for;"

let _assignment_incorrect_2 = "a * b := 5;"

let%expect_test "assignment_incorrect_1" =
    pp_program _assignment_incorrect_1;
    [%expect {| Error: Invalid_expression |}]

;;

let%expect_test "assignment_incorrect_2" =
    pp_program _assignment_incorrect_2;
    [%expect {| Error: Invalid_statement |}]

;;

let _while_correct_1 = "
    while 1 <= 2 do
      a := b + c;
    done"

let%expect_test "while_correct_1" =
    pp_program _while_correct_1;
    [%expect {|
      [(While ((BinOp (Leq, (Int 1), (Int 2))),
          [(Assignment ("a", (BinOp (Add, (Var "b"), (Var "c")))))]))
        ]
      |}]

;;

let _while_correct_2 = "
  while true do
    a := 0;
    while true do
      a := 0;
    done
    a := 0;
  done
  a := 0;"

let%expect_test "while_correct_2" =
    pp_program _while_correct_2;
    [%expect {|
      [(While ((Bool true),
          [(Assignment ("a", (Int 0)));
            (While ((Bool true), [(Assignment ("a", (Int 0)))]));
            (Assignment ("a", (Int 0)))]
          ));
        (Assignment ("a", (Int 0)))]
      |}]

;;

let _while_correct_3 = "
    while \"abc\" + true / 0 do
      a := true + c;
    done"

let%expect_test "while_correct_3" =
    pp_program _while_correct_3;
    [%expect {|
      [(While ((BinOp (Add, (Str "abc"), (BinOp (Div, (Bool true), (Int 0))))),
          [(Assignment ("a", (BinOp (Add, (Bool true), (Var "c")))))]))
        ]
      |}]

;;

let _while_incorrect_1 = "
  while < do
    a := 0;
  done"

let%expect_test "while_incorrect_1" =
    pp_program _while_incorrect_1;
    [%expect {| Error: Invalid_expression |}]

;;

let _while_incorrect_2 = "
  while false do
    a * b := 0;
  done"

let%expect_test "while_incorrect_2" =
    pp_program _while_incorrect_2;
    [%expect {| Error: Invalid_statement |}]

;;

let _ite_correct_1 = "if a == \"123\" then a := 1; fi"

let%expect_test "ite_correct_1" =
  pp_program _ite_correct_1;
  [%expect {|
    [(Ite ((BinOp (Eq, (Var "a"), (Str "123"))), [(Assignment ("a", (Int 1)))],
        []))
      ]
    |}]

;;

let _ite_correct_2 = "if a != a then var a b c; else var abc; fi"

let%expect_test "ite_correct_2" =
  pp_program _ite_correct_2;
  [%expect {|
    [(Ite ((BinOp (Neq, (Var "a"), (Var "a"))), [(Declaration ["a"; "b"; "c"])],
        [(Declaration ["abc"])]))
      ]
    |}]

;;

let _ite_correct_3 = "
if a == 0 then
  if b == 0 then
    c := 0;
    d := 0;
  fi
else
  if g == 0 then
    h := 0;
  fi
fi"

let%expect_test "ite_correct_3" =
  pp_program _ite_correct_3;
  [%expect {|
    [(Ite ((BinOp (Eq, (Var "a"), (Int 0))),
        [(Ite ((BinOp (Eq, (Var "b"), (Int 0))),
            [(Assignment ("c", (Int 0))); (Assignment ("d", (Int 0)))], []))
          ],
        [(Ite ((BinOp (Eq, (Var "g"), (Int 0))), [(Assignment ("h", (Int 0)))],
            []))
          ]
        ))
      ]
    |}]

;;

let _factorial = "
var n acc;
n := 5;
acc := 1;
while n > 1 do
  acc := acc * n;
  n := n - 1;
done"

let%expect_test "factorial" =
    pp_program _factorial;
    [%expect {|
      [(Declaration ["n"; "acc"]); (Assignment ("n", (Int 5)));
        (Assignment ("acc", (Int 1)));
        (While ((BinOp (Gt, (Var "n"), (Int 1))),
           [(Assignment ("acc", (BinOp (Mul, (Var "acc"), (Var "n")))));
             (Assignment ("n", (BinOp (Sub, (Var "n"), (Int 1)))))]
           ))
        ]
      |}]

;;

let _fibonacci = "
var a b n;
n := 5;
a := 0;
b := 1;
while n > 1 do
  b := a + b; 
  a := b - a;
  n := n - 1;
done"

let%expect_test "fibonacci" =
    pp_program _fibonacci;
    [%expect {|
      [(Declaration ["a"; "b"; "n"]); (Assignment ("n", (Int 5)));
        (Assignment ("a", (Int 0))); (Assignment ("b", (Int 1)));
        (While ((BinOp (Gt, (Var "n"), (Int 1))),
           [(Assignment ("b", (BinOp (Add, (Var "a"), (Var "b")))));
             (Assignment ("a", (BinOp (Sub, (Var "b"), (Var "a")))));
             (Assignment ("n", (BinOp (Sub, (Var "n"), (Int 1)))))]
           ))
        ]
      |}]

;;

let _call_expr_correct_1 = "a := fact 1;"

let%expect_test "call_expr_correct_1" =
    pp_program _call_expr_correct_1;
    [%expect {| Error: Invalid_expression |}]

;;

let _call_expr_correct_2 = "
    while 0 == 0 do
      a := fact 1 b;
    done"

let%expect_test "call_expr_correct_2" =
    pp_program _call_expr_correct_2;
    [%expect {| Error: Invalid_expression |}]

;;

let _call_expr_correct_3 = "a := f a 1 + f 1 + f;"

let%expect_test "call_expr_correct_3" =
    pp_program _call_expr_correct_3;
    [%expect {| Error: Invalid_expression |}]

;;

let _call_stmt_correct_1 = "print;"

let%expect_test "call_stmt_correct 1" =
    pp_program _call_stmt_correct_1;
    [%expect {| [(Call ("print", []))] |}]

;;

let _call_stmt_correct_2 = "print 2 2;"

let%expect_test "call_stmt_correct 2" =
    pp_program _call_stmt_correct_2;
    [%expect {| [(Call ("print", [(Int 2); (Int 2)]))] |}]

;;

let _call_stmt_correct_3 = "print (a + b + c) (\"123\" / 0);"

let%expect_test "call_stmt_correct 3" =
    pp_program _call_stmt_correct_3;
    [%expect {|
      [(Call ("print",
          [(BinOp (Add, (BinOp (Add, (Var "a"), (Var "b"))), (Var "c")));
            (BinOp (Div, (Str "123"), (Int 0)))]
          ))
        ]
      |}]

;;
