open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let correct_input_1 = "a == b + 6"
let correct_input_2 = "qwe >= asd / zxc"
let incorrect_input_1 = "a == b == c"
let incorrect_input_2 = "a <"

let parse_boolean_expression_passes_1 () =
  let expected_result = BinOp(Eq, Var(Id "a"), BinOp(Add, Var(Id "b"), Int 6)) in
  let actual_result = tokenize correct_input_1 |> parse_expr_raise_if_rest in
  check bool ("parse_boolean_expression on: " ^ correct_input_1) (actual_result = expected_result) true

let parse_boolean_expression_passes_2 () =
  let expected_result = BinOp(Geq, Var(Id "qwe"), BinOp(Div, Var(Id "asd"), Var(Id "zxc"))) in
  let actual_result = tokenize correct_input_2 |> parse_expr_raise_if_rest in
  check bool ("parse_boolean_expression on: " ^ correct_input_2) (actual_result = expected_result) true

let parse_boolean_expression_raises_1 () =
   try
    ignore (tokenize incorrect_input_1 |> parse_expr_raise_if_rest)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_boolean_expression on: " ^ incorrect_input_1)

let parse_boolean_expression_raises_2 () =
   try
    ignore (tokenize incorrect_input_2 |> parse_expr_raise_if_rest)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_boolean_expression on: " ^ incorrect_input_2)

let tests_for_parse_boolean_expression =
  [
    ("parse_boolean_expression passes on: " ^ correct_input_1, `Quick, parse_boolean_expression_passes_1);
    ("parse_boolean_expression passes on: " ^ correct_input_2, `Quick, parse_boolean_expression_passes_2);
    ("parse_boolean_expression raises on: " ^ incorrect_input_1, `Quick, parse_boolean_expression_raises_1);
    ("parse_boolean_expression raises on: " ^ incorrect_input_2, `Quick, parse_boolean_expression_raises_2);
  ]

let () = run "test_condition.ml"
  [
    ("tests for parse_boolean_expression", tests_for_parse_boolean_expression);
  ]
