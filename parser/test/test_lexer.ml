open Alcotest
open Lexer.Processing
open Lexer.Token

let tokens_of_string_on_keywords () =
  let input = "while for do done var" in
  let expected_result = [WHILE; FOR; DO; DONE; VAR; EOF] in
  let actual_result = tokens_of_string input in
  check bool "tokens_of_string_on_keywords" (expected_result = actual_result) true

let tokens_of_string_on_reserved_lexemes () =
  let input = "== != <= >= := () + - * / ;" in
  let expected_result = [EQ; NEQ; LEQ; GEQ; COLONEQQ; LP; RP; PLUS; MINUS; STAR; SLASH; SEMICOLON; EOF] in
  let actual_result = tokens_of_string input in
  check bool "tokens_of_string_on_keywords" (expected_result = actual_result) true

let tokens_of_string_on_int_and_ids () =
  let input = "a 0 qwe 123" in
  let expected_result = [ID "a"; INT 0; ID "qwe"; INT 123; EOF] in
  let actual_result = tokens_of_string input in
  check bool "tokens_of_string_on_keywords" (expected_result = actual_result) true

  let should_fail_on_invalid_symbols () =
  let input = "_]?" in
  try
    ignore (tokens_of_string input)
  with
  | Invalid_token -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let should_fail_on_empty_string () =
  let input = "" in
  try
    ignore (tokens_of_string input)
  with
  | Invalid_token -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let tests_to_pass =
  [
    ("tokens_of_string_on_keywords", `Quick, tokens_of_string_on_keywords);
    ("tokens_of_string_on_keywords", `Quick, tokens_of_string_on_keywords);
    ("tokens_of_string_on_reserved_lexemes", `Quick, tokens_of_string_on_reserved_lexemes);
    ("tokens_of_string_on_int_and_ids", `Quick, tokens_of_string_on_int_and_ids);
  ]

let tests_to_raise =
  [
    ("should_fail_on_invalid_symbols", `Quick, should_fail_on_invalid_symbols);
    ("should_fail_on_empty_string", `Quick, should_fail_on_empty_string);
  ]

let () = run "lexer-tests" [("tests_to_pass", tests_to_pass); ("tests_to_raise", tests_to_raise)]
