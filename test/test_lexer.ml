open Alcotest
open Lexer.Processing
open Lexer.Token

let correct_input_1 = "while for do done var"

let correct_input_2 = "== != <= >= := () + - * / ;"

let correct_input_3 = "var a := while * for ;"

let incorrect_input_1 = "_]?"

let incorrect_input_2 = String.empty

let tokens_of_string_passes_1 () =
  let expected_result = [WHILE; FOR; DO; DONE; VAR] in
  let actual_result = tokens_of_string correct_input_1 in
  check bool ("tokens_of_string on: " ^ correct_input_1) (expected_result = actual_result) true

let tokens_of_string_passes_2 () =
  let expected_result = [EQ; NEQ; LEQ; GEQ; COLONEQQ; LP; RP; PLUS; MINUS; STAR; SLASH; SEMICOLON] in
  let actual_result = tokens_of_string correct_input_2 in
  check bool ("tokens_of_string on: " ^ correct_input_2) (expected_result = actual_result) true

let tokens_of_string_passes_3 () =
  let expected_result = [VAR; ID "a"; COLONEQQ; WHILE; STAR; FOR; SEMICOLON] in
  let actual_result = tokens_of_string correct_input_3 in
  check bool ("tokens_of_string on: " ^ correct_input_3) (expected_result = actual_result) true

let tokens_of_string_raises_1 () =
  try
    ignore (tokens_of_string incorrect_input_1)
  with
  | Invalid_token -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "tokens_of_string on: " ^ incorrect_input_1)

let tokens_of_string_raises_2 () =
  try
    ignore (tokens_of_string incorrect_input_2)
  with
  | Invalid_token -> ()
  | Failure msg -> failwith msg
  | _ -> failwith ("expected exception did not occur\n" ^ "tokens_of_string on: " ^ incorrect_input_2)

let tests_to_pass =
  [
    ("tokens_of_string passes on: " ^ correct_input_1, `Quick, tokens_of_string_passes_1);
    ("tokens_of_string passes on: " ^ correct_input_2, `Quick, tokens_of_string_passes_2);
    ("tokens_of_string passes on: " ^ correct_input_3, `Quick, tokens_of_string_passes_3);
  ]

let tests_to_raise =
  [
    ("tokens_of_string raises on: " ^ incorrect_input_1, `Quick, tokens_of_string_raises_1);
    ("tokens_of_string raises on: String.empty", `Quick, tokens_of_string_raises_2);
  ]

let () = run "test_lexer.ml"
  [
    ("tests_to_pass", tests_to_pass);
    ("tests_to_raise", tests_to_raise);
  ]
