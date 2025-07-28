open Types
open Lexer
open Parser
open Inferencer
open Generator
open Printf

let _start_code =
"
.global _start
.section .text

_start:

"

let _exit_code =
"

li a0, 0
li a7, 93
ecall
"

let compile text =
  match text |> tokenize with
  | Error err -> Error ("Error: " ^ (show_lexer_error err))
  | Ok tokens ->
  match tokens |> parse_to_program with
  | Error err -> Error ("Error: " ^ (show_parser_error err))
  | Ok program ->
  match program |> infer_types with
  | Error err -> Error ("Error: " ^ (show_inferencer_error err))
  | Ok typed_program ->
    let generated_code = assembly_of_typed_program typed_program in
    Ok (_start_code ^ generated_code ^ _exit_code)

let clear_on_failure = function
    | 0 -> ()
    | _ ->
        Sys.command "rm program program.o stdlib.o program.s program.lang" |> ignore;
        raise Generation_error

;;

let emulator =
    match Sys.getenv_opt "EMULATOR" with
    | Some value -> value
    | None -> failwith "export EMULATOR\n" ^ "\"spike pk\" or \"qemu-riscv64\" or something else"

let save_as text path =
    let oc = open_out path in
    output_string oc text;
    close_out oc;

;;

let run input =
    let destination = "program.s" in
    match compile input with
    | Error msg -> (Sys.command @@ sprintf "echo %s" msg |> ignore;)
    | Ok code ->
    (save_as code destination;
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o program.o" destination |> clear_on_failure;
    Sys.command @@ sprintf "riscv64-unknown-elf-ld program.o -o program" |> clear_on_failure;
    Sys.command @@ sprintf "%s program" emulator |> clear_on_failure;)

;;
