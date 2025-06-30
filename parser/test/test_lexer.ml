open Alcotest
open Lexer.Processing
open Lexer.Token

let simple_correct_expression () =
  let input = "while x != 5" in
  let expected_result = [WHILE; ID "x"; NEQ; INT 5; EOF] in
  let actual_result = get_tokens_of_expression input in
  check bool "parse on: let x = 123 * y in" (expected_result = actual_result) true

let should_fail_on_invalid_symbols () =
  let input = "_]?" in
  try
    ignore (get_tokens_of_expression input)
  with
  | Invalid_token _ -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let should_fail_on_empty_string () =
  let input = "" in
  try
    ignore (get_tokens_of_expression input)
  with
  | Invalid_token _ -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let test_cases =
  [
    ("simple_correct_expression", `Quick, simple_correct_expression);
    ("should_fail_on_invalid_symbols", `Quick, should_fail_on_invalid_symbols);
    ("should_fail_on_empty_string", `Quick, should_fail_on_empty_string);
  ]

let () = run "lexer-tests" [("_", test_cases)]
