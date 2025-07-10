open Alcotest
open Parser.Types
open Parser.Parse_to_program
open Lexer.Processing

let correct_input_assign_1 = "var a := b ; var c := 5 ;"

let correct_input_assign_2 = "var a := 1 + b * 2 ; "

let incorrect_input_assign_1 = "var a := while * for ; "

let incorrect_input_assign_2 = "var a * b := 5; "

let correct_input_while_1 = "
    while 1 <= 2 do
        var a := b + c;
    done"

let correct_input_while_2 = "
  while 0 == 0 do
    var a := 0;
    while 0 == 0 do
      var a := 0;
    done
    var a := 0;
  done
  var a := 0;"

let incorrect_input_while_1 = "
  while 1 do
    var a := 0;
  done"

let incorrect_input_while_2 = "
  while 0 == 0 do
    var a * b := 0;
  done"

let on_assign_passes_1 () =
  let expected_result = [Assignment("a", Var "b"); Assignment("c", Int 5);] in
  let actual_result = tokens_of_string correct_input_assign_1 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_assign_1) (actual_result = expected_result) true

let on_assign_passes_2 () =
  let expected_result = [Assignment("a", Add(Int 1, Mul(Var "b", Int 2)))] in
  let actual_result = tokens_of_string correct_input_assign_2 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_assign_2) (actual_result = expected_result) true

let on_assign_raises_1 () =
  try
    ignore (tokens_of_string incorrect_input_assign_1 |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur on:\n" ^ incorrect_input_assign_1)

let on_assign_raises_2 () =
  try
    ignore (tokens_of_string incorrect_input_assign_2 |> parse_to_program)
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
    [While(Leq(Int 1, Int 2),[Assignment("a", Add(Var "b", Var "c"));];)] in
  let actual_result = tokens_of_string correct_input_while_1 |> parse_to_program in
  check bool ("parse_to_program on:\n" ^ correct_input_while_1) (actual_result = expected_result) true

let on_while_passes_2 () =
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
  let actual_result = tokens_of_string correct_input_while_2 |> parse_to_program in
  check bool ("parse_to_program on:\n" ^ correct_input_while_2) (actual_result = expected_result) true

let on_while_raises_1 () =
  
  try
    ignore (tokens_of_string incorrect_input_while_1 |> parse_to_program)
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur on:\n" ^ incorrect_input_while_1)

let on_while_raises_2 () =
  
  try
    ignore (tokens_of_string incorrect_input_while_2 |> parse_to_program)
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

let () = run "test_program.ml"
  [
    ("tests on assignment", tests_on_assignment);
    ("tests on while", tests_on_while);
  ]
