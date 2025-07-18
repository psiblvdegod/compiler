open Lexer
open Parser
open Backend
open Printf

let _start_code =
"
.global _start
.section .text

_start:

"

let _exit_code =
"

call exit
"

let compile text =
    let generated_code = text |> tokenize |> parse_to_program |> assembly_of_program in
    _start_code ^ generated_code ^ _exit_code

let clear_on_failure = function
    | 0 -> ()
    | _ ->
        (Sys.command "rm program program.o stdlib.o program.s program.lang" |> ignore;
        failwith "something went wrong")

;;

let repo_root =
    match Sys.getenv_opt "REPO_ROOT" with
    | Some value -> value
    | None -> failwith @@ "export REPO_ROOT\n" ^ "example: ~/Desktop/compiler"

let emulator =
    match Sys.getenv_opt "EMULATOR" with
    | Some value -> value
    | None -> failwith "export EMULATOR\n" ^ "\"spike pk\" or \"qemu-riscv64\" or something else"

let stdlib = sprintf "%s/lib/stdlib.s" repo_root

let save_as text path =
    let oc = open_out path in
    output_string oc text;
    close_out oc;

;;

let run input =
    let destination = "./program.s" in
    (compile input |> save_as) destination;
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o program.o" destination |> clear_on_failure;
    Sys.command @@ sprintf "riscv64-unknown-elf-as %s -o stdlib.o" stdlib |> clear_on_failure;
    Sys.command @@ sprintf "riscv64-unknown-elf-ld program.o stdlib.o -o program" |> clear_on_failure;
    Sys.command @@ sprintf "%s program" emulator |> clear_on_failure;

;;
