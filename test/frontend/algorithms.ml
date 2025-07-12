open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let factorial_code = "
var n acc;
n := 5;
acc := 1;
while n > 1 do
  acc := acc * n;
  n := n - 1;
done"

let fibonacci_code = "
var a b n;
n := 5;
a := 0;
b := 1;
while n > 1 do
  b := a + b; 
  a := b - a;
  n := n - 1;
done"

let factorial_ast =
  [
    Declaration(["n"; "acc"]);
    Assignment("n", Int 5);
    Assignment("acc", Int 1);
    While(Gt(Var "n", Int 1),
    [
      Assignment("acc", Mul(Var "acc", Var "n"));
      Assignment("n", Sub(Var "n", Int 1));
    ]
    )
  ]

let fibonacci_ast =
  [
    Declaration(["a"; "b"; "n"]);
    Assignment("n", Int 5);
    Assignment("a", Int 0);
    Assignment("b", Int 1);
    While(Gt(Var "n", Int 1),
    [
      Assignment("b", Add(Var "a", Var "b"));
      Assignment("a", Sub(Var "b", Var "a"));
      Assignment("n", Sub(Var "n", Int 1));
    ]
    )
  ]

let parse_to_program_on_factorial () =
  let input = factorial_code in
  let expected_result = factorial_ast in 
  let actual_result = input |> tokenize |> parse_to_program in
  check bool ("parse_to_program on: " ^ input) (expected_result = actual_result) true

let parse_to_program_on_fibonacci () =
  let input = fibonacci_code in
  let expected_result = fibonacci_ast in 
  let actual_result = input |> tokenize |> parse_to_program in
  check bool ("parse_to_program on: " ^ input) (expected_result = actual_result) true

let iterative_tests =
  [ 
    ("parse_to_program on iterative factorial", `Quick, parse_to_program_on_factorial);
    ("parse_to_program on iterative fibonacci", `Quick, parse_to_program_on_fibonacci);
  ]

let () = run "test_functions.ml"
  [
    ("iterative_tests", iterative_tests);
  ]
