open Alcotest
open Lexer.Processing
open Parser.Types
open Parser.Parse_expression

let correct_input_1 = "1 + 2 * 3"

let correct_input_2 = "asd + zxc * 42 + qwe" 

let correct_input_3 = "a * (b + c)"

let correct_input_4 = "a - (b + c) * (1 / 2)"

let parse_expression_passes_1 () =
  let expected_result = Add(Int 1, Mul(Int 2, Int 3)) in
  let actual_result = tokens_of_string correct_input_1 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_1) (actual_result = expected_result) true

let parse_expression_passes_2 () =
  let expected_result = Add(Add(Var "asd", Mul(Var "zxc", Int 42)), Var "qwe") in
  let actual_result = tokens_of_string correct_input_2 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_2) (actual_result = expected_result) true

let parse_expression_passes_3 () =
  let expected_result = Mul(Var "a", Add(Var "b", Var "c")) in
  let actual_result = tokens_of_string correct_input_3 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_3) (actual_result = expected_result) true

let parse_expression_passes_4 () =
  let expected_result = Sub(Var "a", Mul(Add(Var "b", Var "c"), Div(Int 1, Int 2))) in
  let actual_result = tokens_of_string correct_input_4 |> parse_expression in
  check bool ("parse_expression on: " ^ correct_input_4) (actual_result = expected_result) true

let tests_to_pass =
  [
    ("parse_expression passes on: " ^ correct_input_1, `Quick, parse_expression_passes_1);
    ("parse_expression passes on: " ^ correct_input_2, `Quick, parse_expression_passes_2);
    ("parse_expression passes on: " ^ correct_input_3, `Quick, parse_expression_passes_3);
    ("parse_expression passes on: " ^ correct_input_4, `Quick, parse_expression_passes_4);
  ]

let () = run "test_parser.ml"
  [
    ("parse_expression_passes", tests_to_pass);
  ]
