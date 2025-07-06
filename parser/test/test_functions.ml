open Alcotest
open Parser.Parse_to_program
open Lexer.Processing
open Parser.Types

let should_pass_1 () =
  let input = "var a := fact 1 ;" in
  let expected_result = [Assignment("a", Call("fact", [Int 1]))] in 
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test1" (expected_result = actual_result) true

let should_pass_2 () =
  let input = "while 0 == 0 do var a := fact 1 b ; done" in
  let expected_result = [While(Eq(Int 0, Int 0),[Assignment("a", Call("fact", [Int 1; Var "b"]))])] in 
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test2" (expected_result = actual_result) true

let should_pass_3 () =
  let input = "var a := f a 1 + f 1 + f ;" in
  let expected_result =
    [Assignment("a", Add(Add(Call("f", [Var "a"; Int 1]), Call("f", [Int 1])), Var "f"))] in 
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test1" (expected_result = actual_result) true

let should_raise_1 () =
  let input = "var func a := 1" in
   try
    ignore (tokens_of_string input |> parse_to_program)
  with
  | Invalid_statement -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let should_raise_2 () =
  let input = "var a := fact for while do" in
   try
    ignore (tokens_of_string input |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let tests_to_pass =
  [ 
    ("test_to_pass_1", `Quick, should_pass_1);
    ("test_to_pass_2", `Quick, should_pass_2);
    ("test_to_pass_3", `Quick, should_pass_3);
  ]

let tests_to_raise =
  [ 
    ("test_to_raise_1", `Quick, should_raise_1);
    ("test_to_raise_2", `Quick, should_raise_2);
  ]

let () = run "test_functions"
  [
    ("tests_to_pass", tests_to_pass);
    ("tests_to_raise", tests_to_raise);
  ]
