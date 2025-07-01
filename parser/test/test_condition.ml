open Parser.Parse_condition
open Parser.Types

let parse_condition_should_pass_1 () =
  let input = "a == b + 6" in
  let expected_result = Eq(Var "a", Add(Var "b", Int 6)) in
  let actual_result = parse_condition (tokens_of_string input) in
  check bool "test1" (actual_result = expected_result) true

let parse_condition_should_pass_2 () =
  let input = "qwe >= asd / zxc" in
  let expected_result = Geq(Var "qwe", Div(Var "asd", Var "zxc")) in
  let actual_result = parse_condition (tokens_of_string input) in
  check bool "test1" (actual_result = expected_result) true

let parse_condition_should_raise_1 () =
  let input = "a == b == c" in
   try
    ignore (parse_condition (tokens_of_string input))
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"

let parse_condition_should_raise_2 () =
  let input = "a <" in
   try
    ignore (parse_condition (tokens_of_string input))
  with
  | Invalid_expression -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "expected exception did not occur"


let parse_condition_tests =
  [
    ("test for parse_condition to pass", `Quick, parse_condition_should_pass_1);
    ("test for parse_condition to pass", `Quick, parse_condition_should_pass_2);
    ("test for parse_condition to raise", `Quick, parse_condition_should_raise_1);
    ("test for parse_condition to raise", `Quick, parse_condition_should_raise_2);
  ]

let () = run "all tests"
  [
    ("tests for parse_condition", parse_condition_tests);
  ]
