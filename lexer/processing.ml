open Str
open Token

let parse_keyword = function
  | "while" -> Some WHILE 
  | "done"  -> Some DONE 
  | "var"   -> Some VAR
  | "for"   -> Some FOR 
  | "do"    -> Some DO 
  | _       -> None

let parse_two_chars = function
  | "<="    -> Some LEQ 
  | ">="    -> Some GEQ 
  | "!="    -> Some NEQ 
  | "=="    -> Some EQ 
  | ":="    -> Some COLONEQQ
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

let is_identifier str = string_match (regexp "^[a-z]+$") str 0

let skip str len  = String.sub str len (String.length str - len)

let take str len = String.sub str 0 len

let try_one str =
  if str = String.empty then None else
  match parse_char str.[0] with
  | Some token -> Some(token, skip str 1)
  | None -> None

let try_two str =
  let len = String.length str in
  if len < 2 then None else
    match take str 2 |> parse_two_chars with
    | Some token -> Some(token, skip str 2)
    | None -> None

let rec try_more str len =
  if len = 0 then None else
    let sub_str = take str len in
    let rest = skip str len in
    match int_of_string_opt sub_str with
    | Some n -> Some(INT n, rest)
    | None ->
    if is_identifier sub_str then Some(ID sub_str, rest) else try_more str (len - 1)

let rec parse_loop str result =
  let len = String.length str in
  if len = 0 then result else
  match try_two str with
  | Some (token, rest) -> parse_loop rest (token :: result)
  | None ->
  (match try_one str with
  | Some (token, rest) -> parse_loop rest (token :: result)
  | None ->
  (match try_more str len with
  | Some (token, rest) -> parse_loop rest (token :: result)
  | None -> raise Invalid_token))

let parse_str str =
  match parse_keyword str with
  | Some token -> [token]
  | None -> parse_loop str []

let tokens_of_string str =
  let strs = split (regexp "[ \n\t\r]+") str in
  List.map (fun s -> s |> parse_str |> List.rev) strs |> List.flatten
