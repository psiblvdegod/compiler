open Alcotest
open Parser.Types
open Parser.Parse_to_program
open Lexer.Processing

let assignment_should_pass_1 () =
  let input = "var a := b ; var c := 5 ;" in
  let expected_result = [Assignment("a", Var "b"); Assignment("c", Int 5);] in
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test1" (actual_result = expected_result) true

let assignment_should_pass_2 () =
  let input = "var a := 1 + b * 2 ; " in
  let expected_result = [Assignment("a", Add(Int 1, Mul(Var "b", Int 2)))] in
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test2" (actual_result = expected_result) true

let assignment_should_raise_1 () =
  let input = "var a := while * for; " in
  try
    ignore (tokens_of_string input |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let assignment_should_raise_2 () =
  let input = "var a * b := 5 ; " in
  try
    ignore (tokens_of_string input |> parse_to_program)
  with
  | Invalid_statement -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let assignment_tests =
  [
    ("test for assignment to pass 1", `Quick, assignment_should_pass_1);
    ("test for assignment to pass 2", `Quick, assignment_should_pass_2);

    ("test for assignment to raise 1", `Quick, assignment_should_raise_1);
    ("test for assignment to raise 2", `Quick, assignment_should_raise_2);
  ]

let while_should_pass_1 () =
  let input =
    "
    while 1 <= 2 do
        var a := b + c ;
    done"
  in
  let expected_result =
    [
      While(Leq(Int 1, Int 2),
        [Assignment("a", Add(Var "b", Var "c"));];
      )
    ]  
  
  in
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test1" (actual_result = expected_result) true

let while_should_pass_2 () =
  let input =
    "
    while 0 == 0 do
      var a := 0 ;
      while 0 == 0 do
        var a := 0 ;
      done
      var a := 0 ;
    done
    var a := 0 ;
    "
  in
  let expected_result =
    [
      While(Eq(Int 0, Int 0),
      [
        Assignment("a", Int 0);
        While(Eq(Int 0, Int 0),
          [
            Assignment("a", Int 0)
          ]);
        Assignment("a", Int 0);
      ]);
      Assignment("a", Int 0);
    ]
  in
  let actual_result = tokens_of_string input |> parse_to_program in
  check bool "test2" (actual_result = expected_result) true

let while_should_raise_1 () =
  let input =
    "while 1 do
        var a := 0 ;
     done"
  in
  try
    ignore (tokens_of_string input |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let while_should_raise_2 () =
  let input =
    "while 0 == 0 do
        var a * b := 0 ;
     done"
  in
  try
    ignore (tokens_of_string input |> parse_to_program)
  with
  | Invalid_statement -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let while_tests_to_pass =
  [
    ("test for while to pass 1", `Quick, while_should_pass_1);
    ("test for while to pass 2", `Quick, while_should_pass_2);

    ("test for while to raise 1", `Quick, while_should_raise_1);
    ("test for while to raise 2", `Quick, while_should_raise_2);
  ]

let () = run "test_program.ml"
  [
    ("tests for assignment", assignment_tests);
    ("tests for while", while_tests_to_pass);
  ]
