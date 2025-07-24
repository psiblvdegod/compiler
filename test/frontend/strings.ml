open Alcotest
open Compiler.Lexer
open Compiler.Token

let correct_input = "a := \"while\";"

let tokenize_passes_1 () =
  let expected_result = [ID "a"; COLONEQQ; STR "while"; SEMICOLON] in
  let actual_result = tokenize correct_input in
  check bool ("tokenize on: " ^ correct_input) (expected_result = actual_result) true

let tests_to_pass =
  [
    ("tokenize raises on: " ^ correct_input, `Quick, tokenize_passes_1);
  ]

let () = run "strings.ml"
  [
    ("tests_to_pass", tests_to_pass);
  ]
