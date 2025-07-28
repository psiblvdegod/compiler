open Compiler

let print_str_test =
"
var a;
a := \"12345678\";
"

let%expect_test "print_str_test" =
    App.run print_str_test;
    [%expect {| 12345678 |}]

;;
