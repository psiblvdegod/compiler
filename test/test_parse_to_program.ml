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
  let expected_result = [Assignment("a", Var "b"); Assignment("c", Int 5);] in
  let actual_result = tokenize correct_input_assign_1 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_assign_1) (actual_result = expected_result) true

let on_assign_passes_2 () =
  let expected_result = [Assignment("a", Add(Int 1, Mul(Var "b", Int 2)))] in
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
    [While(Leq(Int 1, Int 2),[Assignment("a", Add(Var "b", Var "c"));];)] in
  let actual_result = tokenize correct_input_while_1 |> parse_to_program in
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

let correct_input_ite_1 = "if a == 0 then a := 1; fi"

let correct_input_ite_2 = "if a != a then var a b c; else var abc; fi"

let correct_input_ite_3 = "
while a != 0 do
  if a < 0 then a := a + 1; else a := a - 1; fi
done
"

let on_ite_passes_1 () =
  let expected_result = [Ite(Eq(Var "a", Int 0), [Assignment("a", Int 1)], [])] in
  let actual_result = tokenize correct_input_ite_1 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_ite_1) (actual_result = expected_result) true

let on_ite_passes_2 () =
  let expected_result = [Ite(Neq(Var "a", Var "a"), [Declaration(["a"; "b"; "c"])], [Declaration(["abc"])])] in
  let actual_result = tokenize correct_input_ite_2 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_ite_2) (actual_result = expected_result) true

let on_ite_passes_3 () =
  let expected_result =
    [While(Neq(Var "a", Int 0), [Ite(Lt(Var "a", Int 0), [Assignment("a", Add(Var "a", Int 1))], [Assignment("a", Sub(Var "a", Int 1))])])] in
  let actual_result = tokenize correct_input_ite_3 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_ite_3) (actual_result = expected_result) true

let tests_on_ite =
  [
    ("parse_to_program passes on:\n" ^ correct_input_ite_1, `Quick, on_ite_passes_1);
    ("parse_to_program passes on:\n" ^ correct_input_ite_2, `Quick, on_ite_passes_2);
    ("parse_to_program passes on:\n" ^ correct_input_ite_2, `Quick, on_ite_passes_3);
  ]

let () = run "test_program.ml"
  [
    ("tests on assignment", tests_on_assignment);
    ("tests on while", tests_on_while);
    ("tests on ite", tests_on_ite);
  ]
