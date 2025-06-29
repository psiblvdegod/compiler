open Alcotest

let test_to_pass () =
  check bool "test to pass" (true) true

let test_cases =
  [
    ("test to pass", `Quick, test_to_pass);
  ]

let () = run "_" [("_", test_cases)]
