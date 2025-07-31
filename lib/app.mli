(* psiblvdegod, 2025, under MIT License *)

(* compiles fictional language code to riscv64 Assembly *)
(* evaluates to Assembly code or error message *)
val compile : string -> (string, string) result

(* executes Assembly code *)
(*
    requires:
    riscv64-unknown-elf-as
    riscv64-unknown-elf-as
*)
val run : string -> unit
