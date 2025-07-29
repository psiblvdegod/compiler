open Compiler

let _print =
"
var a b;
a := 1;
b := 2;
printn a b (a + b);
"

let%expect_test "print" =
  App.run _print;
  [%expect {|
    1
    2
    3
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

let _xor =
"
var a;
a := 0;
printn \"xor :\";

while a < 2 do
    var b;
    b := 0;

    while b < 2 do
      var xor;
      xor := a + b;      

      print \" a = \"   (a == 1) \"\t\";
      print \" b = \"   (b == 1) \"\t\";
      print \" xor = \" (xor == 1) \"\n\";

      b := b + 1;
    done
    a := a + 1;
done

"

let%expect_test "xor" =
    App.run _xor;
    [%expect {|
      xor :
       a = false	 b = false	 xor = false
       a = false	 b = true	 xor = true
       a = true	 b = false	 xor = true
       a = true	 b = true	 xor = false
      |}]

;;



(* declaration *)

let declaration_error =
"
var a a;
"

let%expect_test "declaration_should_fail" =
  App.run declaration_error;
  [%expect {| Error: Already_declared |}];

;;

let declaration_fixed_scope_bug =
"
var a;
a := 1;
printn a;
if a == 1 then
    var a;
    a := 2;
    printn a;
fi
printn a;
"

let%expect_test "declaration_fixed_scope_bug" =
  App.run declaration_fixed_scope_bug;
  [%expect {|
    1
    2
    1
    |}];

;;
