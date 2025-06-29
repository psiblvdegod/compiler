open Alcotest

let test_to_pass () =
  check bool "test_to_pass" (true) true

let test_to_fail () =
  check bool "test_to_pass" (true) false

let suite = [("test_to_pass",`Quick, test_to_pass);("test_to_fail",`Quick, test_to_fail);]

let () = run "tests" [("tests", suite);]
