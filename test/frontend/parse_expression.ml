open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let correct_input_1 = "1 + 2 * 3"

let correct_input_2 = "asd + zxc * 42 + qwe" 

let correct_input_3 = "a * (b + c)"

let correct_input_4 = "a - (b + c) * (1 / 2)"

let parse_expression_passes_1 () =
  let expected_result = BinOp(Add, Int 1, BinOp(Mul, Int 2, Int 3)) in
  let actual_result = tokenize correct_input_1 |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ correct_input_1) (actual_result = expected_result) true

let parse_expression_passes_2 () =
  let expected_result = BinOp(Add, BinOp(Add, Var (Id "asd"), BinOp(Mul, Var (Id "zxc"), Int 42)), Var (Id "qwe")) in
  let actual_result = tokenize correct_input_2 |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ correct_input_2) (actual_result = expected_result) true

let parse_expression_passes_3 () =
  let expected_result = BinOp(Mul, Var (Id "a"), BinOp(Add, Var (Id "b"), Var (Id "c"))) in
  let actual_result = tokenize correct_input_3 |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ correct_input_3) (actual_result = expected_result) true

let parse_expression_passes_4 () =
  let expected_result = BinOp(Sub, Var (Id "a"), BinOp(Mul, BinOp(Add, Var (Id "b"), Var (Id "c")), BinOp(Div, Int 1, Int 2))) in
  let actual_result = tokenize correct_input_4 |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ correct_input_4) (actual_result = expected_result) true

let correct_with_negative_1 = "-a + b / c - -d"

let correct_with_negative_2 = "-a * b"

let correct_with_negative_3 = "-(-a) * -(b + -c) / -2"

let parse_with_negative_1 () =
  let input = correct_with_negative_1 in
  let expected_result = BinOp(Sub, BinOp(Add, UnOp(Neg, Var (Id "a")), BinOp(Div, Var (Id "b"), Var (Id "c"))), UnOp(Neg, Var (Id "d"))) in
  let actual_result = tokenize input |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ input) (actual_result = expected_result) true

let parse_with_negative_2 () =
  let input = correct_with_negative_2 in
  let expected_result = BinOp(Mul, UnOp(Neg, Var (Id "a")), Var (Id "b")) in
  let actual_result = tokenize input |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ input) (actual_result = expected_result) true

let parse_with_negative_3 () =
  let input = correct_with_negative_3 in
  let expected_result = BinOp(Div, BinOp(Mul,UnOp(Neg, UnOp(Neg, Var (Id "a"))), UnOp(Neg, BinOp(Add, Var (Id "b"), UnOp(Neg, Var (Id "c"))))), UnOp(Neg, Int 2)) in
  let actual_result = tokenize input |> parse_expr_raise_if_rest in
  check bool ("parse_expression on: " ^ input) (actual_result = expected_result) true

let tests_to_pass =
  [
    ("parse_expression passes on: " ^ correct_input_1, `Quick, parse_expression_passes_1);
    ("parse_expression passes on: " ^ correct_input_2, `Quick, parse_expression_passes_2);
    ("parse_expression passes on: " ^ correct_input_3, `Quick, parse_expression_passes_3);
    ("parse_expression passes on: " ^ correct_input_4, `Quick, parse_expression_passes_4);
  ]

let tests_for_Neg_to_pass =
  [
    ("parse_expression passes on: " ^ correct_with_negative_1, `Quick, parse_with_negative_1);
    ("parse_expression passes on: " ^ correct_with_negative_2, `Quick, parse_with_negative_2);
    ("parse_expression passes on: " ^ correct_with_negative_3, `Quick, parse_with_negative_3);
  ]

let () = run "test_parse_expression.ml"
  [
    ("parse_expression_passes", tests_to_pass);
    ("tests_for_Neg_to_pass", tests_for_Neg_to_pass);
  ]
