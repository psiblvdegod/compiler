open Alcotest
open Compiler.Lexer
open Compiler.Token

let correct_input_1 = "while for do done var"

let correct_input_2 = "== != <= >= := () + - * / ;"

let correct_input_3 = "var a := while * for ;"

let incorrect_input_1 = "_]?"

let incorrect_input_2 = String.empty

let tokenize_passes_1 () =
  let expected_result = [WHILE; FOR; DO; DONE; VAR] in
  let actual_result = tokenize correct_input_1 in
  check bool ("tokenize on: " ^ correct_input_1) (expected_result = actual_result) true

let tokenize_passes_2 () =
  let expected_result = [EQ; NEQ; LEQ; GEQ; COLONEQQ; LP; RP; PLUS; MINUS; STAR; SLASH; SEMICOLON] in
  let actual_result = tokenize correct_input_2 in
  check bool ("tokenize on: " ^ correct_input_2) (expected_result = actual_result) true

let tokenize_passes_3 () =
  let expected_result = [VAR; ID "a"; COLONEQQ; WHILE; STAR; FOR; SEMICOLON] in
  let actual_result = tokenize correct_input_3 in
  check bool ("tokenize on: " ^ correct_input_3) (expected_result = actual_result) true

let tokenize_raises_1 () =
  try
    ignore (tokenize incorrect_input_1)
  with
  | Invalid_token -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "tokenize on: " ^ incorrect_input_1)

let tokenize_raises_2 () =
  try
    ignore (tokenize incorrect_input_2)
  with
  | Invalid_token -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "tokenize on: " ^ incorrect_input_2)

let tests_to_pass =
  [
    ("tokenize passes on: " ^ correct_input_1, `Quick, tokenize_passes_1);
    ("tokenize passes on: " ^ correct_input_2, `Quick, tokenize_passes_2);
    ("tokenize passes on: " ^ correct_input_3, `Quick, tokenize_passes_3);
  ]

let tests_to_raise =
  [
    ("tokenize raises on: " ^ incorrect_input_1, `Quick, tokenize_raises_1);
    ("tokenize raises on: String.empty", `Quick, tokenize_raises_2);
  ]

let () = run "test_lexer.ml"
  [
    ("tests_to_pass", tests_to_pass);
    ("tests_to_raise", tests_to_raise);
  ]
