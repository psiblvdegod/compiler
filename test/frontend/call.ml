open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let correct_input_1 = "a := fact 1;"
let correct_input_2 = "
    while 0 == 0 do
      a := fact 1 b;
    done"
let correct_input_3 = "a := f a 1 + f 1 + f;"
let incorrect_input_1 = "func a := 1"
let incorrect_input_2 = "a := fact for while do"

let parse_to_program_passes_1 () =
  let expected_result = [Assignment("a", Call("fact", [Int 1]))] in 
  let actual_result = tokenize correct_input_1 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_1) (expected_result = actual_result) true

let parse_to_program_passes_2 () =
  let expected_result = [While(Comparison(Eq, Int 0, Int 0),[Assignment("a", Call("fact", [Int 1; Var "b"]))])] in 
  let actual_result = tokenize correct_input_2 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_2) (expected_result = actual_result) true

let parse_to_program_passes_3 () =
  let expected_result =
    [Assignment("a", BinOp(Add, BinOp(Add, Call("f", [Var "a"; Int 1]), Call("f", [Int 1])), Var "f"))] in 
  let actual_result = tokenize correct_input_3 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_3) (expected_result = actual_result) true

let parse_to_program_raises_1 () =
   try
    ignore (tokenize incorrect_input_1 |> parse_to_program)
  with
  | Invalid_statement -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_to_program on: " ^ incorrect_input_1)

let parse_to_program_raises_2 () =
   try
    ignore (tokenize incorrect_input_2 |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "parse_to_program on: " ^ incorrect_input_2)

let tests_to_pass =
  [ 
    ("parse_to_program passes on:\n" ^ correct_input_1, `Quick, parse_to_program_passes_1);
    ("parse_to_program passes on:\n" ^ correct_input_2, `Quick, parse_to_program_passes_2);
    ("parse_to_program passes on:\n" ^ correct_input_3, `Quick, parse_to_program_passes_3);
  ]

let tests_to_raise =
  [ 
    ("parse_to_program raises on:\n" ^ incorrect_input_1, `Quick, parse_to_program_raises_1);
    ("parse_to_program raises on:\n" ^ incorrect_input_2, `Quick, parse_to_program_raises_2);
  ]

let () = run "test_call.ml"
  [
    ("tests_to_pass", tests_to_pass);
    ("tests_to_raise", tests_to_raise);
  ]
