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

type lexer_state = {
  str : string;
  len : int;
  pos : int;
}

let addi_pos state n = { str = state.str; len = state.len; pos = state.pos + n }

let init_state input = { str = input; len = String.length input; pos = 0 }

let try_1 state =
  if state.pos = state.len then None else
  match parse_char state.str.[state.pos] with
  | Some token -> Some (addi_pos state 1, token)
  | None -> None

let try_2 state =
  if state.len - state.pos < 2 then None else
  match parse_two_chars (state.str.[state.pos], state.str.[state.pos + 1]) with
  | Some token -> Some (addi_pos state 2, token)
  | None -> None

let rec take_int state index =
  (* contains updated state and token as lazy pair; cause otherwise program may crash *)
  let result = lazy(addi_pos state (index - state.pos), INT(String.sub state.str state.pos (index - state.pos) |> int_of_string)) in
  if index = state.len then result
    else match state.str.[index] with
    | '0'..'9' -> take_int state (index + 1)
    | 'a'..'z' -> raise Invalid_token
    | _ -> result


let rec take_str state index =
  let new_state = addi_pos state (index - state.pos) in
  if index = state.len then
      let str = String.sub state.str state.pos (index - state.pos) in
      match token_of_keyword str with
      | Some token -> new_state, token
      | None -> new_state, ID str

    else match state.str.[index] with
    | 'a'..'z' -> take_str state (index + 1)
    | '0'..'9' -> raise Invalid_token
    | _ ->
      let str = String.sub state.str state.pos (index - state.pos) in
      match token_of_keyword str with
      | Some token ->
        (match state.str.[index] with
        | ' ' | '(' | '\n' | '\t' | '\r' -> new_state, token
        | _ -> raise Invalid_token)
      | None -> new_state, ID str

let try_n state =
  if state.pos = state.len then None else
  match state.str.[state.pos] with
  | '0'..'9' -> Some (take_int state state.pos |> Lazy.force)
  | 'a'..'z' -> Some (take_str state state.pos)
  | _ -> None

let is_unsignificant state = String.contains " \n\r\t" (state.str.[state.pos])

let rec parse_loop state acc =
  if state.pos = state.len then acc else
  if is_unsignificant state then parse_loop (addi_pos state 1) acc else
  match try_2 state with
  | Some (state, token) -> parse_loop state (token :: acc)
  | None ->
  (match try_1 state with
  | Some (state, token) -> parse_loop state (token :: acc)
  | None ->
  (match try_n state with
  | Some (state, token) -> parse_loop state (token :: acc)
  | None -> raise Invalid_token))

let tokens_of_string str =
  parse_loop (init_state str) [] |> List.rev
