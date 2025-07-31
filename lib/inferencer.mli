(* psiblvdegod, 2025, under MIT License *)

open Types

(* annotates AST (type 'program') *)
(* adds information about available variables and procedures to each statement. *)
(* adds information about type to each expression. *)
val infer_types : program -> (typed_program, inferencer_error) result
