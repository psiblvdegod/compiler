(* psiblvdegod, 2025, under MIT License *)

open Types

(* annotates AST (type 'program') *)
val infer_types : program -> (typed_program, inferencer_error) result
