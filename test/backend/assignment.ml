open Stdio
open Compiler.Types
open Compiler.Backend

let%expect_test "assignment test" =
  let program = [Declaration(["a"]); Assignment("a", Int 5)] in
  assembly_of_program program |> printf "%s";
  [%expect {|
    .global _start
    .section .text

    _start:
    addi sp, sp, -16
    addi sp, sp, -16
    li t1, 5
    sw t1, 0(sp)
    lw t1, 0(sp)
    addi sp, sp, 16
    sw t1, 0(sp)

    li a7, 93
    li a0, 0
    ecall
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
    addi sp, sp, -16
    li t1, 5
    sw t1, 0(sp)
    lw t1, 0(sp)
    addi sp, sp, 16
    sw t1, 32(sp)
    lw t1, 32(sp)
    addi sp, sp, -16
    sw t1, 0(sp)
    lw t1, 0(sp)
    addi sp, sp, 16
    sw t1, 16(sp)

    li a7, 93
    li a0, 0
    ecall
    |}]

let%expect_test "addition test" =
  let program = [Declaration(["a"]); Assignment("a", Add(Int 10, Int 20))] in
  assembly_of_program program |> printf "%s";
  [%expect {|
    .global _start
    .section .text

    _start:
    addi sp, sp, -16
    addi sp, sp, -16
    li t1, 10
    sw t1, 0(sp)
    addi sp, sp, -16
    li t1, 20
    sw t1, 0(sp)
    lw t1, 16(sp)
    lw t2, 0(sp)
    add t1, t1, t2
    addi sp, sp, 16
    sw t1, 0(sp)
    lw t1, 0(sp)
    addi sp, sp, 16
    sw t1, 0(sp)

    li a7, 93
    li a0, 0
    ecall
    |}]

let%expect_test "addition test" =
  let program = [Declaration(["a"]); Assignment("a", Add(Int 10, Add(Var "a", Int 20)))] in
  assembly_of_program program |> printf "%s";
  [%expect {|
    .global _start
    .section .text

    _start:
    addi sp, sp, -16
    addi sp, sp, -16
    li t1, 10
    sw t1, 0(sp)
    lw t1, 16(sp)
    addi sp, sp, -16
    sw t1, 0(sp)
    addi sp, sp, -16
    li t1, 20
    sw t1, 0(sp)
    lw t1, 16(sp)
    lw t2, 0(sp)
    add t1, t1, t2
    addi sp, sp, 16
    sw t1, 0(sp)
    lw t1, 16(sp)
    lw t2, 0(sp)
    add t1, t1, t2
    addi sp, sp, 16
    sw t1, 0(sp)
    lw t1, 0(sp)
    addi sp, sp, 16
    sw t1, 0(sp)

    li a7, 93
    li a0, 0
    ecall
    |}]
