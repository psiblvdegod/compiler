open Token

let token_of_keyword = function
  | "while" -> Some WHILE 
  | "done"  -> Some DONE 
  | "var"   -> Some VAR
  | "do"    -> Some DO
  | "if"    -> Some IF
  | "then"  -> Some THEN
  | "else"  -> Some ELSE
  | "fi"    -> Some FI
  | _       -> None

let token_of_two_chars = function
  | '<', '='    -> Some LEQ 
  | '>', '='    -> Some GEQ 
  | '!', '='    -> Some NEQ 
  | '=', '='    -> Some EQ 
  | ':', '='    -> Some COLONEQQ
  | _           -> None

let token_of_char = function
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

type lexer_state = {
  str : string;
  len : int;
  pos : int;
}

let addi_pos state n = { str = state.str; len = state.len; pos = state.pos + n }

let init_state input = { str = input; len = String.length input; pos = 0 }

let rec token_of_int state index =
  let new_state = addi_pos state (index - state.pos) in
  let token = lazy (INT(String.sub state.str state.pos (index - state.pos) |> int_of_string)) in
  if index = state.len then new_state, token
  else match state.str.[index] with
  | '0'..'9' -> token_of_int state (index + 1)
  | 'a'..'z' -> raise Invalid_token
  | _ -> new_state, token

let rec token_of_id state index =
  let new_state = addi_pos state (index - state.pos) in
  let str = String.sub state.str state.pos (index - state.pos) in
  match token_of_keyword str with
  | Some token ->
    (if index = state.len then new_state, token else
      match state.str.[index] with
      | 'a'..'z' -> token_of_id state (index + 1)
      | ' ' | '(' | ')' | '\n' | '\t' | '\r' | ';' -> new_state, token
      | _ -> raise Invalid_token)
  | None -> 
    (if index = state.len then new_state, ID str else
      match state.str.[index] with
      | 'a'..'z' -> token_of_id state (index + 1)
      | '0'..'9' -> raise Invalid_token
      | _ -> new_state, ID str)

let try_tokenize_char state =
  if state.pos = state.len then None else
  match token_of_char state.str.[state.pos] with
  | Some token -> Some (addi_pos state 1, token)
  | None -> None

let try_tokenize_two_chars state =
  if state.len - state.pos < 2 then None else
  match token_of_two_chars (state.str.[state.pos], state.str.[state.pos + 1]) with
  | Some token -> Some (addi_pos state 2, token)
  | None -> None

let try_tokenize_more state =
  if state.pos = state.len then None else
  match state.str.[state.pos] with
  | '0'..'9' -> let state, token = token_of_int state state.pos in Some (state, token |> Lazy.force)
  | 'a'..'z' -> Some (token_of_id state state.pos)
  | _ -> None

let is_unsignificant state =
  match state.str.[state.pos] with
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let rec tokenize_loop state acc =
  if state.pos = state.len then acc else
  if is_unsignificant state then tokenize_loop (addi_pos state 1) acc else
  match try_tokenize_two_chars state with
  | Some (state, token) -> tokenize_loop state (token :: acc)
  | None ->
  match try_tokenize_char state with
  | Some (state, token) -> tokenize_loop state (token :: acc)
  | None ->
  match try_tokenize_more state with
  | Some (state, token) -> tokenize_loop state (token :: acc)
  | None -> raise Invalid_token

let tokenize str = tokenize_loop (init_state str) [] |> List.rev
