open Compiler.Token
open Compiler.Lexer

let correct_input_1 = "while do done var"
let correct_input_2 = "== != <= >= := () + - * /;"
let correct_input_3 = "var a := while * do;"
let correct_input_4 = "if a > 1 then a := b; else a := c; fi;"
let incorrect_input_1 = "_]?"
let incorrect_input_2 = String.empty
let incorrect_input_3 = "\"fdf"

let pp_tokens text =
  match tokenize text with
  | Error err -> print_endline ("Error: " ^ show_error err)
  | Ok(tokens) -> List.iter (fun s -> print_endline (show_token s)) tokens

let%expect_test "test on correct input 1" =
  pp_tokens correct_input_1;
  [%expect {|
    WHILE
    DO
    DONE
    VAR
    |}];

;;

let%expect_test "test on correct input 2" =
  pp_tokens correct_input_2;
  [%expect {|
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
    SEMICOLON
    |}];

;;

let%expect_test "test on correct input 3" =
  pp_tokens correct_input_3;
  [%expect {|
    VAR
    (ID "a")
    COLONEQQ
    WHILE
    STAR
    DO
    SEMICOLON
    |}];

;;

let%expect_test "test on correct input 4" =
  pp_tokens correct_input_4;
  [%expect {|
    IF
    (ID "a")
    GT
    (INT 1)
    THEN
    (ID "a")
    COLONEQQ
    (ID "b")
    SEMICOLON
    ELSE
    (ID "a")
    COLONEQQ
    (ID "c")
    SEMICOLON
    FI
    SEMICOLON
    |}];

;;

let%expect_test "test on incorrect input 1" =
  pp_tokens incorrect_input_1;
  [%expect {| Error: Invalid_token |}];

;;

let%expect_test "test on incorrect input 2" =
  pp_tokens incorrect_input_2;
  [%expect {| Error: Input_is_empty |}];

;;

let%expect_test "test on incorrect input 3" =
  pp_tokens incorrect_input_3;
  [%expect {| Error: Invalid_token |}];

;;
