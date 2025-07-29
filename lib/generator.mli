(* psiblvdegod, 2025, under MIT License *)

open Types

(* uses annotated AST (type 'typed_program') to generate riscv64 Assembly *)
(* raises Not_implemented and Not_supported *)
val assembly_of_typed_program : typed_program -> string
