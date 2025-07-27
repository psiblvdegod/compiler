open Compiler

let test1 =
"
var a b;
a := 1;
b := 10;
a := a + b;
print a;
"

let%expect_test "test1" =
  App.run test1;
  [%expect {|
    addi sp, sp, -64
    li t1, 1
    addi sp, sp, -32
    sw t1, (sp)
    lw t1, (sp)
    addi sp, sp, 32
    sw t1, 32(sp)
    |}];

;;
