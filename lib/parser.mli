(* psiblvdegod, 2025, under MIT License *)

open Types

(* constructs AST (type 'program') of token sequence *)
val parse_to_program : token list -> (program, parser_error) result

(* constructs AST (type 'expression') of token sequence *)
(* it is a part of the public interface for testing purposes. *)
val parse_expression : token list -> (expression, parser_error) result
