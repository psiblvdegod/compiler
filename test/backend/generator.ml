open Compiler.Types
open Compiler.Lexer
open Compiler.Parser
open Compiler.Inferencer
open Compiler.Generator

let compile str = 
  match str |> tokenize with
  | Error err -> print_endline ("Error: " ^ (show_lexer_error err))
  | Ok(tokens) ->
    match tokens |> parse_to_program with
    | Error err -> print_endline ("Error: " ^ (show_parser_error err))
    | Ok(program) ->
      match program |> infer_types with
      | Error err -> print_endline ("Error: " ^ (show_inferencer_error err))
      | Ok(typed_program) -> print_endline (assembly_of_typed_program typed_program)

let test1 =
"
var a b;
a := 1;
"

let%expect_test "test1" =
  compile test1;
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

let test2 =
"
var a;
a := 1 + 5;
"
let%expect_test "test2" =
  compile test2;
  [%expect {|
    addi sp, sp, -32
    li t1, 1
    addi sp, sp, -32
    sw t1, (sp)
    li t1, 5
    addi sp, sp, -32
    sw t1, (sp)
    lw t1, 32(sp)
    lw t2, 0(sp)
    add t1, t1, t2
    addi sp, sp, 32
    sw t1, 0(sp)
    lw t1, (sp)
    addi sp, sp, 32
    sw t1, 0(sp)
    |}];

;;


let test1 =
"
var a;
a := 1;
a := 10 + 5 * a;

"
let%expect_test "test1" =
  compile test1;
  [%expect {|
    addi sp, sp, -32
    li t1, 1
    addi sp, sp, -32
    sw t1, (sp)
    lw t1, (sp)
    addi sp, sp, 32
    sw t1, 0(sp)
    li t1, 10
    addi sp, sp, -32
    sw t1, (sp)
    li t1, 5
    addi sp, sp, -32
    sw t1, (sp)
    lw t1, 64(sp)
    addi sp, sp, -32
    sw t1, (sp)
    lw t1, 32(sp)
    lw t2, 0(sp)
    mul t1, t1, t2
    addi sp, sp, 32
    sw t1, 0(sp)
    lw t1, 32(sp)
    lw t2, 0(sp)
    add t1, t1, t2
    addi sp, sp, 32
    sw t1, 0(sp)
    lw t1, (sp)
    addi sp, sp, 32
    sw t1, 0(sp)
    |}];

;;
