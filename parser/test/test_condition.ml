open Parser.Parse_condition
open Parser.Types
open Lexer.Processing
open Alcotest

let correct_input_1 = "a == b + 6"
let correct_input_2 = "qwe >= asd / zxc"
let incorrect_input_1 = "a == b == c"
let incorrect_input_2 = "a <"

let parse_condition_passes_1 () =
  let expected_result = Eq(Var "a", Add(Var "b", Int 6)) in
  let actual_result = tokens_of_string correct_input_1 |> parse_condition in
  check bool ("parse_condition on: " ^ correct_input_1) (actual_result = expected_result) true

let parse_condition_passes_2 () =
  let expected_result = Geq(Var "qwe", Div(Var "asd", Var "zxc")) in
  let actual_result = tokens_of_string correct_input_2 |> parse_condition in
  check bool ("parse_condition on: " ^ correct_input_2) (actual_result = expected_result) true

let parse_condition_raises_1 () =
   try
    ignore (tokens_of_string incorrect_input_1 |> parse_condition)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_condition on: " ^ incorrect_input_1)

let parse_condition_raises_2 () =
   try
    ignore (tokens_of_string incorrect_input_2 |> parse_condition)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_condition on: " ^ incorrect_input_2)


let tests_for_parse_condition =
  [
    ("parse_condition passes on: " ^ correct_input_1, `Quick, parse_condition_passes_1);
    ("parse_condition passes on: " ^ correct_input_2, `Quick, parse_condition_passes_2);
    ("parse_condition raises on: " ^ incorrect_input_1, `Quick, parse_condition_raises_1);
    ("parse_condition raises on: " ^ incorrect_input_2, `Quick, parse_condition_raises_2);
  ]

let () = run "test_condition.ml"
  [
    ("tests for parse_condition", tests_for_parse_condition);
  ]
