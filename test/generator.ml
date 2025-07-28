open Compiler

let test1 =
"
var a b c;
a := 1;
b := 2;
c := 3;
printn a b c;
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
printn 1 b (a + b);
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
printn (1 + 2 * 3) (-4) (-5 - (-6) - (-(-1)));
"

let%expect_test "test3" =
  App.run test3;
  [%expect {|
    7
    -4
    0
    |}];

;;

let _factorial = "
var left right;
left := -7;
right := 8;

var current;
current := left;

while current < right do
    
    var n acc sign;
    n := current;
    acc := 1;
    sign := n < 0;

    while n != 0 do
        acc := acc * n;  
        if
          sign
        then
          n := n + 1;
        else 
          n := n - 1;
        fi
    done

    current := current + 1;

    printn acc;
done
"

let%expect_test "factorial" =
    App.run _factorial;
    [%expect {|
      -5040
      720
      -120
      24
      -6
      2
      -1
      1
      1
      2
      6
      24
      120
      720
      5040
      |}]

;;

let _fibonacci = "
var a b n;

n := 7;
a := 0;
b := 1;

while n > 1 do
  b := a + b; 
  a := b - a;
  n := n - 1;

  printn b;

done
"

let%expect_test "fibonacci" =
    App.run _fibonacci;
    [%expect {|
      1
      2
      3
      5
      8
      13
      |}]

;;

(* tests on Type_Str *)

let print_str_test =
"
var a;
a := \"12345678\";
printn a;
"

let%expect_test "print_str_test" =
    App.run print_str_test;
    [%expect {| 12345678 |}]

;;
