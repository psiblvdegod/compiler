open Str
open Token

let token_of_keyword = function
  | "while" -> Some WHILE 
  | "done"  -> Some DONE 
  | "var"   -> Some VAR
  | "for"   -> Some FOR 
  | "do"    -> Some DO 
  | _       -> None

let parse_two_chars = function
  | '<', '='    -> Some LEQ 
  | '>', '='    -> Some GEQ 
  | '!', '='    -> Some NEQ 
  | '=', '='    -> Some EQ 
  | ':', '='    -> Some COLONEQQ
  | _       -> None

let parse_char = function
  | '<'     -> Some LT
  | '>'     -> Some GT
  | '('     -> Some LP 
  | ')'     -> Some RP 
  | ';'     -> Some SEMICOLON 
  | '+'     -> Some PLUS 
  | '-'     -> Some MINUS 
  | '*'     -> Some STAR 
  | '/'     -> Some SLASH
  | _       -> None

let is_digit = function
| '0'..'9' -> true
| _ -> false

let is_letter = function
| 'a'..'z' -> true
| _ -> false

type lexer_state = {
  str : string;
  len : int;
  pos : int;
}

let change_pos state new_pos = { str = state.str; len = state.len; pos = new_pos }

let init_state input = { str = input; len = String.length input; pos = 0 }

let try_1 state =
  if state.pos = state.len then None else
  match parse_char state.str.[state.pos] with
  | Some token -> Some (change_pos state (state.pos + 1), token)
  | None -> None

let try_2 state =
  if state.len - state.pos < 2 then None else
  match parse_two_chars (state.str.[state.pos], state.str.[state.pos + 1]) with
  | Some token -> Some (change_pos state (state.pos + 2), token)
  | None -> None

let rec take_int state index =
  if index = state.len
    then change_pos state index, INT(String.sub state.str state.pos (index - state.pos) |> int_of_string)
    else match state.str.[index] with
    | '0'..'9' -> take_int state (index + 1)
    | 'a'..'z' -> raise Invalid_token
    | _ -> change_pos state index, INT(String.sub state.str state.pos (index - state.pos) |> int_of_string)

let rec take_str state index =
  if index = state.len
    then change_pos state index, String.sub state.str state.pos (index - state.pos)
    else match state.str.[index] with
    | 'a'..'z' -> take_str state (index + 1)
    | '0'..'9' -> raise Invalid_token
    | _ -> change_pos state index, String.sub state.str state.pos (index - state.pos)

let try_n state =
  if state.pos = state.len then None else
  match state.str.[state.pos] with
  | '0'..'9' -> Some (take_int state state.pos)
  | 'a'..'z' ->
    (let state, str = take_str state state.pos in
    match token_of_keyword str with
    | Some token -> Some (state, token)
    | None -> Some (state, ID str))
  | _ -> None

let rec parse_loop state acc =
  if state.pos = state.len then acc else
  match try_2 state with
  | Some (state, token) -> parse_loop state (token :: acc)
  | None ->
  (match try_1 state with
  | Some (state, token) -> parse_loop state (token :: acc)
  | None ->
  (match try_n state with
  | Some (state, token) -> parse_loop state (token :: acc)
  | None -> raise Invalid_token))

let parse_str state =
  match token_of_keyword state.str with
  | Some token -> [token]
  | None -> parse_loop state []

let tokens_of_string str =
  let strs = split (regexp "[ \n\t\r]+") str in
  List.map (fun s -> s |> init_state |> parse_str |> List.rev) strs |> List.flatten
