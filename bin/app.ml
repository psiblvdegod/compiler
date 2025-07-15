open Compiler.Parser
open Compiler.Lexer
open Compiler.Backend

let _start_code =
"
.section .text
.global _start

_start:
call save_sp

# generated code
# =======================================

"

let _exit_code =
"
# =======================================

call print_stack
call exit
"

let assembly_of_string str =
  _start_code ^ (str |> tokenize |> parse_to_program |> assembly_of_program) ^ _exit_code

let () =
  let input_channel = Sys.argv.(1) |> open_in in
  let source = in_channel_length input_channel |> really_input_string input_channel in
  let output_channel = Sys.argv.(2) |> open_out in
  assembly_of_string source |> output_string output_channel;
  close_in input_channel;
  close_out output_channel;
