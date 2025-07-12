open Stdio
open Compiler.Types
open Compiler.Backend

let%expect_test "assignment test" =
  let program = [Declaration(["a"]);Assignment("a", Int 5)] in
  assembly_of_program program |> printf "%s";
  [%expect {|
    .global _start
    .section .text

    _start:
    addi sp, sp, -16
    li t1, 5
    sw t1, 0(sp)
    |}]

let%expect_test "assignment test" =
  let program = [Declaration(["a"; "b"; "c"]); Assignment("a", Int 5); Assignment("b", Var "a")] in
  assembly_of_program program |> printf "%s";
  [%expect {|
    .global _start
    .section .text

    _start:
    addi sp, sp, -16
    addi sp, sp, -16
    addi sp, sp, -16
    li t1, 5
    sw t1, 32(sp)
    lw t1, 32(sp)
    sw t1, 16(sp)
    |}]
