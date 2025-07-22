open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let correct_input_assign_1 = "a := b; c := 5;"

let correct_input_assign_2 = "a := 1 + b * 2;"

let incorrect_input_assign_1 = "a := while * for;"

let incorrect_input_assign_2 = "a * b := 5;"

let correct_input_while_1 = "
    while 1 <= 2 do
      a := b + c;
    done"

let correct_input_while_2 = "
  while 0 == 0 do
    a := 0;
    while 0 == 0 do
      a := 0;
    done
    a := 0;
  done
  a := 0;"

let incorrect_input_while_1 = "
  while 1 do
    a := 0;
  done"

let incorrect_input_while_2 = "
  while 0 == 0 do
    a * b := 0;
  done"

let on_assign_passes_1 () =
  let expected_result = [Assignment(Id "a", Var (Id "b")); Assignment(Id "c", Int 5);] in
  let actual_result = tokenize correct_input_assign_1 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_assign_1) (actual_result = expected_result) true

let on_assign_passes_2 () =
  let expected_result = [Assignment(Id "a", BinOp(Add, Int 1, BinOp(Mul, Var (Id "b"), Int 2)))] in
  let actual_result = tokenize correct_input_assign_2 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_assign_2) (actual_result = expected_result) true

let on_assign_raises_1 () =
  try
    ignore (tokenize incorrect_input_assign_1 |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur on:\n" ^ incorrect_input_assign_1)

let on_assign_raises_2 () =
  try
    ignore (tokenize incorrect_input_assign_2 |> parse_to_program)
  with
  | Invalid_statement -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur on:\n" ^ incorrect_input_assign_2)

let tests_on_assignment =
  [
    ("parse_to_program passes on: " ^ correct_input_assign_1, `Quick, on_assign_passes_1);
    ("parse_to_program passes on: " ^ correct_input_assign_2, `Quick, on_assign_passes_2);

    ("parse_to_program raises on: " ^ incorrect_input_assign_1, `Quick, on_assign_raises_1);
    ("parse_to_program raises on: " ^ incorrect_input_assign_2, `Quick, on_assign_raises_2);
  ]

let on_while_passes_1 () =
  let expected_result =
    [While(BinOp(Leq, Int 1, Int 2),[Assignment(Id "a", BinOp(Add, Var (Id "b"), Var (Id "c")));];)] in
  let actual_result = tokenize correct_input_while_1 |> parse_to_program in
  check bool ("parse_to_program on:\n" ^ correct_input_while_1) (actual_result = expected_result) true

let on_while_passes_2 () =
  let expected_result =
    [
      While(BinOp(Eq, Int 0, Int 0),
      [
        Assignment(Id "a", Int 0);
        While(BinOp(Eq, Int 0, Int 0),
          [
            Assignment(Id "a", Int 0)
          ]);
        Assignment(Id "a", Int 0);
      ]);
      Assignment(Id "a", Int 0);
    ]
  in
  let actual_result = tokenize correct_input_while_2 |> parse_to_program in
  check bool ("parse_to_program on:\n" ^ correct_input_while_2) (actual_result = expected_result) true

let on_while_raises_1 () =
  
  try
    ignore (tokenize incorrect_input_while_1 |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur on:\n" ^ incorrect_input_while_1)

let on_while_raises_2 () =
  
  try
    ignore (tokenize incorrect_input_while_2 |> parse_to_program)
  with
  | Invalid_statement -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur on:\n" ^ incorrect_input_while_2)

let tests_on_while =
  [
    ("parse_to_program passes on:\n" ^ correct_input_while_1, `Quick, on_while_passes_1);
    ("parse_to_program passes on:\n" ^ correct_input_while_2, `Quick, on_while_passes_2);

    ("parse_to_program raises on:\n" ^ incorrect_input_while_1, `Quick, on_while_raises_1);
    ("parse_to_program raises on:\n" ^ incorrect_input_while_2, `Quick, on_while_raises_2);
  ]


let () = run "test_parse_to_program.ml"
  [
    ("tests on assignment", tests_on_assignment);
    ("tests on while", tests_on_while);
  ]
