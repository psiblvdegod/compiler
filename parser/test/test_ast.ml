open Alcotest
open Lexer.Parse
open Parse
open Expression

let addition_test () =
  let input = "1 + 2 + 3" in
  let tokens = get_tokens_of_expression input in
  let expected_result = (Addition(Addition(Integer 1, Integer 2), Integer 3)) in
  let actual_result = parse_to_ast tokens in
  check bool "1 + 2 + 3" (actual_result = expected_result) true

let multiplication_test () =
  let input = "1 * 2 * 3" in
  let tokens = get_tokens_of_expression input in
  let expected_result = (Multiplication(Multiplication(Integer 1, Integer 2), Integer 3)) in
  let actual_result = parse_to_ast tokens in
  check bool "1 * 2 * 3" (actual_result = expected_result) true

let add_and_mul_test () =
  let input = "1 + 2 * 3" in
  let tokens = get_tokens_of_expression input in
  let expected_result = Addition(Integer 1, Multiplication(Integer 2, Integer 3)) in
  let actual_result = parse_to_ast tokens in
  check bool "1 + 2 * 3" (actual_result = expected_result) true

let add_and_mul_with_vars_test () =
  let input = "asd + zxc * 42 + qwe" in
  let tokens = get_tokens_of_expression input in
  let expected_result = Addition(Addition(Variable "asd", Multiplication(Variable "zxc", Integer 42)), Variable "qwe") in
  let actual_result = parse_to_ast tokens in
  check bool "asd + zxc * 42 + qwe" (actual_result = expected_result) true

let test_cases =
  [
    ("addition_test", `Quick, addition_test);
    ("multiplication_test", `Quick, multiplication_test);
    ("add_and_mul_test", `Quick, add_and_mul_test);
    ("add_and_mul_with_vars_test", `Quick, add_and_mul_with_vars_test);
  ]

let () = run "all tests" [("binary operations", test_cases)]
