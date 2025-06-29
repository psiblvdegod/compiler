open Alcotest

let test_to_pass () =
  check bool "test_to_pass" (true) true

let suite = [("test_to_pass",`Quick, test_to_pass);]

let () = run "tests" [("test_to_pass", suite);]
