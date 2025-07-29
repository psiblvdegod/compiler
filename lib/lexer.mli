(* psiblvdegod, 2025, under MIT License *)

open Types

(* splits fictional language code to token sequence *)
val tokenize : string -> (token list, lexer_error) result
