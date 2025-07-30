(* psiblvdegod, 2025, under MIT License *)

open Compiler

let execute input =
  match App.compile input with
  | Error msg -> print_endline msg
  | Ok code -> App.run code

let _print = "
var a b;
a := 1;
b := 2;
printn a b (a + b);
"

let%expect_test "print" =
  execute _print;
  [%expect {|
    1
    2
    3
    |}]

let _factorial =
  "
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
  execute _factorial;
  [%expect
    {|
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

let _fibonacci =
  "
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
  execute _fibonacci;
  [%expect {|
      1
      2
      3
      5
      8
      13
      |}]

let _xor =
  "
var a b;
a := true;
b := false;
var xor;
xor := (a or b) and !(a and b);
print xor;
"

let%expect_test "xor" =
  execute _xor;
  [%expect {| true |}]

let _definition_hello_world =
  "
def helloworld =>
  print \"hello world!!!\";
end

helloworld;
"

let%expect_test "definition_hello_world" =
  execute _definition_hello_world;
  [%expect {| hello world!!! |}]

let _definition_hello_world =
  "
def fact (int n) =>
  var acc;
  acc := 1;
  while (n != 0) do
    acc := acc * n;
    if (n < 0) then
      n := n + 1;
    else
      n := n - 1;
    fi
  done 
  printn acc;
end

var j;
j := -7;
while j < 8 do
  fact j;
  j := j + 1;
done
"

let%expect_test "definition_hello_world" =
  execute _definition_hello_world;
  [%expect {| hello world!!! |}]
