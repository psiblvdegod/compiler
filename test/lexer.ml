(* psiblvdegod, 2025, under MIT License *)

open Compiler.Types
open Compiler.Lexer

let pp_tokens text =
  match tokenize text with
  | Error err -> print_endline ("Error: " ^ show_lexer_error err)
  | Ok tokens -> List.iter (fun s -> print_endline (show_token s)) tokens

let correct_input_1 = "while do done var"

let%expect_test "test on correct input 1" =
  pp_tokens correct_input_1;
  [%expect {|
    WHILE
    DO
    DONE
    VAR
    |}]

let correct_input_2 = "== != <= >= := () + - * /;"

let%expect_test "test on correct input 2" =
  pp_tokens correct_input_2;
  [%expect
    {|
    EQ
    NEQ
    LEQ
    GEQ
    COLONEQQ
    LP
    RP
    PLUS
    MINUS
    STAR
    SLASH
    SEMI
    |}]

let correct_input_3 = "var a := while * do;"

let%expect_test "test on correct input 3" =
  pp_tokens correct_input_3;
  [%expect
    {|
    VAR
    (ID "a")
    COLONEQQ
    WHILE
    STAR
    DO
    SEMI
    |}]

let correct_input_4 = "if a > 1 then a := b; else a := c; fi;"

let%expect_test "test on correct input 4" =
  pp_tokens correct_input_4;
  [%expect
    {|
    IF
    (ID "a")
    GT
    (INT 1)
    THEN
    (ID "a")
    COLONEQQ
    (ID "b")
    SEMI
    ELSE
    (ID "a")
    COLONEQQ
    (ID "c")
    SEMI
    FI
    SEMI
    |}]

let incorrect_input_1 = "_]?"

let%expect_test "test on incorrect input 1" =
  pp_tokens incorrect_input_1;
  [%expect {| Error: Invalid_token |}]

let incorrect_input_2 = String.empty

let%expect_test "test on incorrect input 2" =
  pp_tokens incorrect_input_2;
  [%expect {| Error: Input_is_empty |}]

let incorrect_input_3 = "\"fdf"

let%expect_test "test on incorrect input 3" =
  pp_tokens incorrect_input_3;
  [%expect {| Error: Invalid_token |}]

let correct_input_5 = "~a and -b or !c"

let%expect_test "test on correct input 5" =
  pp_tokens correct_input_5;
  [%expect
    {|
    TILDE
    (ID "a")
    AND
    MINUS
    (ID "b")
    OR
    BANG
    (ID "c")
    |}]
