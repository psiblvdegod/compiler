open Compiler.Parser
open Compiler.Lexer
open Compiler.Backend

let _start_code =
"
.section .text
.global _start

_start:

# generated code
# =======================================

"

let _exit_code =
"
# =======================================

call exit
"

let compile str =
  _start_code ^ (str |> tokenize |> parse_to_program |> assembly_of_program) ^ _exit_code

let () =
  let input_channel = Sys.argv.(1) |> open_in in
  let source = in_channel_length input_channel |> really_input_string input_channel in
  let output_channel = Sys.argv.(2) |> open_out in
  compile source |> output_string output_channel;
  close_in input_channel;
  close_out output_channel;
