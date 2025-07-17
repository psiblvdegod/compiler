open Lexer
open Parser
open Backend

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

let compile input_channel output_channel =
    let source = really_input_string input_channel @@ in_channel_length input_channel in
    let generated_code = source |> tokenize |> parse_to_program |> assembly_of_program in
    let result = _start_code ^ generated_code ^ _exit_code in
    output_string output_channel result;
