open Alcotest
open Lexer.Processing
open Parser.Types
open Parser.Parse_expression

let add_test () =
  let input = "1 + 2 + 3" in
  let expected_result = (Add(Add(Int 1, Int 2), Int 3)) in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "1 + 2 + 3" (actual_result = expected_result) true

let mul_test () =
  let input = "1 * 2 * 3" in
  let expected_result = (Mul(Mul(Int 1, Int 2), Int 3)) in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "1 * 2 * 3" (actual_result = expected_result) true

let add_and_mul_test () =
  let input = "1 + 2 * 3" in
  let expected_result = Add(Int 1, Mul(Int 2, Int 3)) in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "1 + 2 * 3" (actual_result = expected_result) true

let add_and_mul_with_vars_test () =
  let input = "asd + zxc * 42 + qwe" in
  let expected_result = Add(Add(Var "asd", Mul(Var "zxc", Int 42)), Var "qwe") in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "asd + zxc * 42 + qwe" (actual_result = expected_result) true

let sub_and_div_test () =
  let input = "a / b - 1 - 2 / c" in
  let expected_result = Sub(Sub(Div(Var "a", Var "b"), Int 1), Div(Int 2, Var "c")) in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "1 + 2 + 3" (actual_result = expected_result) true

let parentheses_test1 () =
  let input = "a * (b + c)" in
  let expected_result = Mul(Var "a", Add(Var "b", Var "c")) in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "1 + 2 + 3" (actual_result = expected_result) true

let parentheses_test2 () =
  let input = "a - (b + c) * (1 / 2)" in
  let expected_result = Sub(Var "a", Mul(Add(Var "b", Var "c"), Div(Int 1, Int 2))) in
  let actual_result = tokens_of_string input |> parse_expression in
  check bool "1 + 2 + 3" (actual_result = expected_result) true

let other_tests =
  [
    ("parentheses_test1", `Quick, parentheses_test1);
    ("parentheses_test1", `Quick, parentheses_test2);
  ]

let _binary_operations_tests =
  [
    ("add_test", `Quick, add_test);
    ("mul_test", `Quick, mul_test);
    ("add_and_mul_test", `Quick, add_and_mul_test);
    ("add_and_mul_with_vars_test", `Quick, add_and_mul_with_vars_test);
    ("sub_and_div_test", `Quick, sub_and_div_test);
  ]

let () = run "test_parser.ml"
  [
    ("other tests", other_tests);
  ]
