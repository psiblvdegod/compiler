open Printf
open Compiler.App

let repo_root = Sys.getenv "REPO_ROOT"

let stdlib = sprintf "%s/lib/stdlib.s" repo_root

let run input =
    let src = "./program.lang" in
    let dest = "./program.s" in
    let oc = open_out src in
    output_string oc input;
    close_out oc;
    let ic = open_in src in
    let oc = open_out dest in
    compile ic oc;
    close_in ic;
    close_out oc;
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o program.o" dest |> ignore;
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o stdlib.o" stdlib |> ignore;
    Sys.command @@ sprintf "riscv64-unknown-elf-ld program.o stdlib.o -o program" |> ignore;
    Sys.command @@ sprintf "spike pk program" |> ignore;

;;

let factorial =
"
var n acc;

n := 7;
acc := 1;

while n > 0 do
    acc := acc * n;
    n := n - 1;
done

print acc;
"

let%expect_test "factorial" =
  run factorial;
  [%expect {| 5040 |}];

;;

let fibonacci =
"
var a b n;

n := 7;

a := 0;
b := 1;

while n != 0 do
    b := b + a;
    a := b - a;
    n := n - 1;
done

print b;
"

let%expect_test "fibonacci" =
  run fibonacci;
  [%expect {| 21 |}];

;;

let xor =
"
var a b xor;

a := 0;
b := 1;

if a == 0 then
    if b == 0 then
        xor := 0;
    else
        xor := 1;
    fi
else
    if b == 0 then
        xor := 1;
    else
        xor := 0;
    fi
fi

print xor;
"

let%expect_test "xor" =
  run xor;
  [%expect {| 1 |}];

;;

let bubble_sort_on_4 =
"
var a b c d issorted tmp;

a := 2;
b := 3;
c := 1;
d := 0;

issorted := 0;

while issorted != 1 do
    issorted := 1;  
    if a > b then
        tmp := a;
        a := b;
        b := tmp;
        issorted := 0;
    fi

    if b > c then
        tmp := b;
        b := c;
        c := tmp;
        issorted := 0;
    fi

    if c > d then
        tmp := c;
        c := d;
        d := tmp;
        issorted := 0;
    fi
done

print a b c d;
"

let%expect_test "bubble sort" =
  run bubble_sort_on_4;
  [%expect {|
    0
    1
    2
    3
    |}];

;;
