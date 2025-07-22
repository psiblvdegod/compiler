open Alcotest
open Compiler.Lexer
open Compiler.Parser
open Compiler.Types

let correct_input_ite_1 = "if a == 0 then a := 1; fi"

let correct_input_ite_2 = "if a != a then var a b c; else var abc; fi"

let correct_input_ite_3 = "
while a != 0 do
  if a < 0 then a := a + 1; else a := a - 1; fi
done"

let on_ite_passes_1 () =
  let expected_result = [Ite(BinOp(Eq, Var (Id "a"), Int 0), [Assignment(Id "a", Int 1)], [])] in
  let actual_result = tokenize correct_input_ite_1 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_ite_1) (actual_result = expected_result) true

let on_ite_passes_2 () =
  let expected_result = [Ite(BinOp(Neq, Var (Id "a"), Var (Id "a")), [Declaration([Id "a"; Id "b"; Id "c"])], [Declaration([Id "abc"])])] in
  let actual_result = tokenize correct_input_ite_2 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_ite_2) (actual_result = expected_result) true

let on_ite_passes_3 () =
  let expected_result =
    [While(BinOp(Neq, Var (Id "a"), Int 0), [Ite(BinOp(Lt, Var (Id "a"), Int 0), [Assignment(Id "a", BinOp(Add, Var (Id "a"), Int 1))], [Assignment(Id "a", BinOp(Sub, Var (Id "a"), Int 1))])])] in
  let actual_result = tokenize correct_input_ite_3 |> parse_to_program in
  check bool ("parse_to_program on: " ^ correct_input_ite_3) (actual_result = expected_result) true

let tests_on_ite =
  [
    ("parse_to_program passes on:\n" ^ correct_input_ite_1, `Quick, on_ite_passes_1);
    ("parse_to_program passes on:\n" ^ correct_input_ite_2, `Quick, on_ite_passes_2);
    ("parse_to_program passes on:\n" ^ correct_input_ite_2, `Quick, on_ite_passes_3);
  ]

let nested_ite = "
if a == 0 then
  if b == 0 then
    c := 0;
    d := 0;
  fi
else
  if g == 0 then
    h := 0;
  fi
fi"

let nested_ite_ast =
  [
    Ite(BinOp(Eq, Var (Id "a"), Int 0),
      [Ite(BinOp(Eq, Var (Id "b"), Int 0),
        [
          Assignment(Id "c", Int 0);
          Assignment(Id "d", Int 0)
        ], [])],
      [Ite(BinOp(Eq, Var (Id "g"), Int 0),
        [
          Assignment(Id "h", Int 0)
        ], [])])
  ]

let on_nested_ite_passes () =
  let input = nested_ite in
  let expected_result = nested_ite_ast in
  let actual_result = input |> tokenize |> parse_to_program in
  check bool ("parse_to_program on:\n" ^ input) (actual_result = expected_result) true

let tests_on_ite_2 =
  [
    ("parse_to_program passes on:\n" ^ nested_ite, `Quick, on_nested_ite_passes);
  ]

let () = run "test_ite.ml"
[
  ("tests on ite", tests_on_ite);
  ("tests on ite 2", tests_on_ite_2);
]
