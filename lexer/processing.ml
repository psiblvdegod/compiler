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
  if index = state.len || (is_digit state.str.[index] = false)
    then change_pos state index, INT(String.sub state.str state.pos (index - state.pos) |> int_of_string)
    else take_int state (index + 1)

let rec take_id state index =
  if index = state.len || (is_letter state.str.[index] = false)
    then change_pos state index, ID(String.sub state.str state.pos (index - state.pos))
    else take_id state (index + 1)

let try_n state =
  if state.pos = state.len then None else
  match state.str.[state.pos] with
  | '0'..'9' -> Some (take_int state state.pos)
  | 'a'..'z' -> Some (take_id state state.pos)
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
