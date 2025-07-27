open Compiler

let test1 =
"
var a b c;
a := 1;
b := 2;
c := 3;
print a b c;
"

let%expect_test "test1" =
  App.run test1;
  [%expect {|
    1
    2
    3
    |}];

;;

let test2 =
"
var a b;
a := 1;
b := 2;
a := a + b;
print 1 b (a + b);
"

let%expect_test "test2" =
  App.run test2;
  [%expect {|
    1
    2
    5
    |}];

;;

let test3 =
"
print (1 + 2 * 3) (-4) (-5 - (-6) - (-(-1)));
"

let%expect_test "test3" =
  App.run test3;
  [%expect {|
    7
    -4
    0
    |}];

;;
