(* psiblvdegod, 2025, under MIT License *)

open Types

let token_of_reserved = function
  | "while" -> Some WHILE
  | "done" -> Some DONE
  | "var" -> Some VAR
  | "do" -> Some DO
  | "if" -> Some IF
  | "then" -> Some THEN
  | "else" -> Some ELSE
  | "fi" -> Some FI
  | "and" -> Some AND
  | "or" -> Some OR
  | "true" -> Some TRUE
  | "false" -> Some FALSE
  | "define" -> Some DEFINE
  | "end" -> Some END
  | _ -> None

let token_of_two_chars = function
  | '<', '=' -> Some LEQ
  | '>', '=' -> Some GEQ
  | '!', '=' -> Some NEQ
  | '=', '=' -> Some EQ
  | ':', '=' -> Some COLONEQQ
  | '=', '>' -> Some IMPLIES
  | _ -> None

let token_of_char = function
  | '<' -> Some LT
  | '>' -> Some GT
  | '(' -> Some LP
  | ')' -> Some RP
  | ';' -> Some SEMI
  | '+' -> Some PLUS
  | '-' -> Some MINUS
  | '*' -> Some STAR
  | '/' -> Some SLASH
  | '^' -> Some CARET
  | '~' -> Some TILDE
  | '!' -> Some BANG
  | _ -> None

type lexer_state = { str : string; len : int; pos : int }

let addi_pos state n = { str = state.str; len = state.len; pos = state.pos + n }
let set_pos state pos = { str = state.str; len = state.len; pos }
let init_state input = { str = input; len = String.length input; pos = 0 }

let rec token_of_int state index =
  let new_state = addi_pos state (index - state.pos) in
  let token =
    lazy
      (INT (String.sub state.str state.pos (index - state.pos) |> int_of_string))
  in
  if index = state.len then Ok (new_state, token)
  else
    match state.str.[index] with
    | '0' .. '9' -> token_of_int state (index + 1)
    | 'a' .. 'z' -> Error Invalid_token
    | _ -> Ok (new_state, token)

let rec token_of_id state index =
  let new_state = set_pos state index in
  let str = String.sub state.str state.pos (index - state.pos) in
  match token_of_reserved str with
  | Some token -> (
      if index = state.len then Ok (new_state, token)
      else
        match state.str.[index] with
        | 'a' .. 'z' -> token_of_id state (index + 1)
        | ' ' | '\n' | '\t' | '\r' | '(' | ')' | ';' -> Ok (new_state, token)
        | _ -> Error Invalid_token)
  | None -> (
      if index = state.len then Ok (new_state, ID str)
      else
        match state.str.[index] with
        | 'a' .. 'z' -> token_of_id state (index + 1)
        | '0' .. '9' -> Error Invalid_token
        | _ -> Ok (new_state, ID str))

let try_tokenize_char state =
  if state.pos = state.len then None
  else
    match token_of_char state.str.[state.pos] with
    | Some token -> Some (addi_pos state 1, token)
    | None -> None

let try_tokenize_two_chars state =
  if state.len - state.pos < 2 then None
  else
    match
      token_of_two_chars (state.str.[state.pos], state.str.[state.pos + 1])
    with
    | Some token -> Some (addi_pos state 2, token)
    | None -> None

let try_tokenize_more state =
  if state.pos = state.len then None
  else
    match state.str.[state.pos] with
    | '0' .. '9' -> (
        match token_of_int state state.pos with
        | Error err -> Some (Error err)
        | Ok (state, token) -> Some (Ok (state, token |> Lazy.force)))
    | 'a' .. 'z' -> Some (token_of_id state state.pos)
    | _ -> None

let try_tokenize_str_lit state =
  if state.len - state.pos < 2 then None
  else if state.str.[state.pos] <> '\"' then None
  else
    match String.index_from_opt state.str (state.pos + 1) '\"' with
    | None -> Some (Error Invalid_token)
    | Some index ->
        let token =
          STR (String.sub state.str (state.pos + 1) (index - state.pos - 1))
        in
        let state = set_pos state (index + 1) in
        Some (Ok (state, token))

let is_unsignificant state =
  match state.str.[state.pos] with
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let rec tokenize_loop state acc =
  if state.pos = state.len then Ok acc
  else if is_unsignificant state then tokenize_loop (addi_pos state 1) acc
  else
    match try_tokenize_str_lit state with
    | Some (Error err) -> Error err
    | Some (Ok (state, token)) -> tokenize_loop state (token :: acc)
    | None -> (
        match try_tokenize_two_chars state with
        | Some (state, token) -> tokenize_loop state (token :: acc)
        | None -> (
            match try_tokenize_char state with
            | Some (state, token) -> tokenize_loop state (token :: acc)
            | None -> (
                match try_tokenize_more state with
                | Some (Error err) -> Error err
                | Some (Ok (state, token)) -> tokenize_loop state (token :: acc)
                | None -> Error Invalid_token)))

let tokenize str =
  if String.trim str = String.empty then Error Input_is_empty
  else
    match tokenize_loop (init_state str) [] with
    | Error err -> Error err
    | Ok tokens -> Ok (List.rev tokens)
