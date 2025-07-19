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
  let actual_result = tokenize correct_input_1 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_1) (actual_result = expected_result) true

let parse_expression_passes_2 () =
  let expected_result = BinOp(Add, BinOp(Add, Var "asd", BinOp(Mul, Var "zxc", Int 42)), Var "qwe") in
  let actual_result = tokenize correct_input_2 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_2) (actual_result = expected_result) true

let parse_expression_passes_3 () =
  let expected_result = BinOp(Mul, Var "a", BinOp(Add, Var "b", Var "c")) in
  let actual_result = tokenize correct_input_3 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_3) (actual_result = expected_result) true

let parse_expression_passes_4 () =
  let expected_result = BinOp(Sub, Var "a", BinOp(Mul, BinOp(Add, Var "b", Var "c"), BinOp(Div, Int 1, Int 2))) in
  let actual_result = tokenize correct_input_4 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_4) (actual_result = expected_result) true

let correct_with_negative_1 =
  "-a + b / c - -d"

let correct_with_negative_2 = "-a * b"

let correct_with_negative_3 = "-(-a) * -(b + -c) / -2"

let parse_with_negative_1 () =
  let input = correct_with_negative_1 in
  let expected_result = BinOp(Sub, BinOp(Add, Neg(Var "a"), BinOp(Div, Var "b", Var "c")), Neg(Var "d")) in
  let actual_result = tokenize input |> parse_expression in
  check bool ("parse_expression on: " ^ input) (actual_result = expected_result) true

let parse_with_negative_2 () =
  let input = correct_with_negative_2 in
  let expected_result = BinOp(Mul, Neg(Var "a"), Var "b") in
  let actual_result = tokenize input |> parse_expression in
  check bool ("parse_expression on: " ^ input) (actual_result = expected_result) true

let parse_with_negative_3 () =
  let input = correct_with_negative_3 in
  let expected_result = BinOp(Div, BinOp(Mul, Neg(Neg(Var "a")), Neg(BinOp(Add, Var "b", Neg(Var "c")))), Neg(Int 2)) in
  let actual_result = tokenize input |> parse_expression in
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
