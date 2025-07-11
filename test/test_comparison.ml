open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let correct_input_1 = "a == b + 6"
let correct_input_2 = "qwe >= asd / zxc"
let incorrect_input_1 = "a == b == c"
let incorrect_input_2 = "a <"

let parse_comparison_passes_1 () =
  let expected_result = Eq(Var "a", Add(Var "b", Int 6)) in
  let actual_result = tokenize correct_input_1 |> parse_comparison in
  check bool ("parse_comparison on: " ^ correct_input_1) (actual_result = expected_result) true

let parse_comparison_passes_2 () =
  let expected_result = Geq(Var "qwe", Div(Var "asd", Var "zxc")) in
  let actual_result = tokenize correct_input_2 |> parse_comparison in
  check bool ("parse_comparison on: " ^ correct_input_2) (actual_result = expected_result) true

let parse_comparison_raises_1 () =
   try
    ignore (tokenize incorrect_input_1 |> parse_comparison)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_comparison on: " ^ incorrect_input_1)

let parse_comparison_raises_2 () =
   try
    ignore (tokenize incorrect_input_2 |> parse_comparison)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_comparison on: " ^ incorrect_input_2)


let tests_for_parse_comparison =
  [
    ("parse_comparison passes on: " ^ correct_input_1, `Quick, parse_comparison_passes_1);
    ("parse_comparison passes on: " ^ correct_input_2, `Quick, parse_comparison_passes_2);
    ("parse_comparison raises on: " ^ incorrect_input_1, `Quick, parse_comparison_raises_1);
    ("parse_comparison raises on: " ^ incorrect_input_2, `Quick, parse_comparison_raises_2);
  ]

let () = run "test_comparison.ml"
  [
    ("tests for parse_comparison", tests_for_parse_comparison);
  ]
