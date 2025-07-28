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

let _factorial = "
var left right;
left := -7;
right := 8;

var current n acc sign;
current := left;

while current < right do
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

    print acc;
done
"

let%expect_test "factorial" =
    App.run _factorial;
    [%expect {| -120 |}]

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

  print b;

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
