open Compiler

let pp_ast text =
  match Lexer.tokenize text with
  | Error err -> print_endline ("Error: " ^ Token.show_error err)
  | Ok(tokens) ->
  match Parser.parse_expression tokens with
  | Error err -> print_endline ("Error: " ^ Types.show_error err)
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
         (BinOp (Add, (Var (Id "asd")), (BinOp (Mul, (Var (Id "zxc")), (Int 42))))),
         (Var (Id "qwe"))))
      |}]

;;

let _parse_expression_correct_3 = "a * (b + c)"

let%expect_test "parse_expression_correct_3" =
    pp_ast _parse_expression_correct_3;
    [%expect {| (BinOp (Mul, (Var (Id "a")), (BinOp (Add, (Var (Id "b")), (Var (Id "c")))))) |}]

;;

let _parse_expression_correct_4 = "a - (b + c) * (1 / 2)"

let%expect_test "parse_expression_correct_4" =
    pp_ast _parse_expression_correct_4;
    [%expect {|
      (BinOp (Sub, (Var (Id "a")),
         (BinOp (Mul, (BinOp (Add, (Var (Id "b")), (Var (Id "c")))),
            (BinOp (Div, (Int 1), (Int 2)))))
         ))
      |}]

;;

let pp_program text = 
  match Lexer.tokenize text with
  | Error err -> print_endline ("Error: " ^ Token.show_error err)
  | Ok(tokens) ->
  match Parser.parse_to_program tokens with
  | Error err -> print_endline ("Error: " ^ Types.show_error err)
  | Ok(program) -> print_endline(Types.show_program program)

let _assignment_correct_1 = "a := b; c := 5;"

let%expect_test "assignment_correct_1" =
    pp_program _assignment_correct_1;
    [%expect {| [(Assignment ((Id "a"), (Var (Id "b")))); (Assignment ((Id "c"), (Int 5)))] |}]

;;

let _assignment_correct_2 = "a := 1 + b * 2;"

let%expect_test "assignment_correct_1" =
    pp_program _assignment_correct_2;
    [%expect {|
      [(Assignment ((Id "a"),
          (BinOp (Add, (Int 1), (BinOp (Mul, (Var (Id "b")), (Int 2)))))))
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
          [(Assignment ((Id "a"), (BinOp (Add, (Var (Id "b")), (Var (Id "c"))))))]
          ))
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
          [(Assignment ((Id "a"), (Int 0)));
            (While ((Bool true), [(Assignment ((Id "a"), (Int 0)))]));
            (Assignment ((Id "a"), (Int 0)))]
          ));
        (Assignment ((Id "a"), (Int 0)))]
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
          [(Assignment ((Id "a"), (BinOp (Add, (Bool true), (Var (Id "c"))))))]))
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
    [(Ite ((BinOp (Eq, (Var (Id "a")), (Str "123"))),
        [(Assignment ((Id "a"), (Int 1)))], []))
      ]
    |}]

;;

let _ite_correct_2 = "if a != a then var a b c; else var abc; fi"

let%expect_test "ite_correct_2" =
  pp_program _ite_correct_2;
  [%expect {|
    [(Ite ((BinOp (Neq, (Var (Id "a")), (Var (Id "a")))),
        [(Declaration [(Id "a"); (Id "b"); (Id "c")])],
        [(Declaration [(Id "abc")])]))
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
    [(Ite ((BinOp (Eq, (Var (Id "a")), (Int 0))),
        [(Ite ((BinOp (Eq, (Var (Id "b")), (Int 0))),
            [(Assignment ((Id "c"), (Int 0))); (Assignment ((Id "d"), (Int 0)))],
            []))
          ],
        [(Ite ((BinOp (Eq, (Var (Id "g")), (Int 0))),
            [(Assignment ((Id "h"), (Int 0)))], []))
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
      [(Declaration [(Id "n"); (Id "acc")]); (Assignment ((Id "n"), (Int 5)));
        (Assignment ((Id "acc"), (Int 1)));
        (While ((BinOp (Gt, (Var (Id "n")), (Int 1))),
           [(Assignment ((Id "acc"),
               (BinOp (Mul, (Var (Id "acc")), (Var (Id "n"))))));
             (Assignment ((Id "n"), (BinOp (Sub, (Var (Id "n")), (Int 1)))))]
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
      [(Declaration [(Id "a"); (Id "b"); (Id "n")]);
        (Assignment ((Id "n"), (Int 5))); (Assignment ((Id "a"), (Int 0)));
        (Assignment ((Id "b"), (Int 1)));
        (While ((BinOp (Gt, (Var (Id "n")), (Int 1))),
           [(Assignment ((Id "b"), (BinOp (Add, (Var (Id "a")), (Var (Id "b"))))));
             (Assignment ((Id "a"), (BinOp (Sub, (Var (Id "b")), (Var (Id "a"))))));
             (Assignment ((Id "n"), (BinOp (Sub, (Var (Id "n")), (Int 1)))))]
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
    [%expect {| [(Call ((Id "print"), []))] |}]

;;

let _call_stmt_correct_2 = "print 2 2;"

let%expect_test "call_stmt_correct 2" =
    pp_program _call_stmt_correct_2;
    [%expect {| [(Call ((Id "print"), [(Int 2); (Int 2)]))] |}]

;;

let _call_stmt_correct_3 = "print (a + b + c) (\"123\" / 0);"

let%expect_test "call_stmt_correct 3" =
    pp_program _call_stmt_correct_3;
    [%expect {|
      [(Call
          ((Id "print"),
           [(BinOp (Add, (BinOp (Add, (Var (Id "a")), (Var (Id "b")))),
               (Var (Id "c"))));
             (BinOp (Div, (Str "123"), (Int 0)))]))
        ]
      |}]

;;
