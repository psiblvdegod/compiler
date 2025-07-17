open Printf
open Compiler.App

let rr = Sys.getenv "REPO_ROOT"

let stdlib = sprintf "%s/lib/stdlib.s" rr

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
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o ./program.o" dest |> ignore;
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o ./stdlib.o" stdlib |> ignore;
    Sys.command @@ sprintf "riscv64-unknown-elf-ld program.o stdlib.o -o program" |> ignore;
    Sys.command @@ sprintf "spike pk program" |> ignore;

;;

let factorial_input =
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
  run factorial_input;
  [%expect {| 5040 |}];

;;
