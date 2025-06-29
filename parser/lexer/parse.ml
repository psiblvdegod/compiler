let get_tokens_of_expression expression =
  let lexbuf = Lexing.from_string expression in
  let rec inner result =
    match Rule_token.token lexbuf with
    | Token.EOF -> Token.EOF :: result
    | x -> inner (x :: result)
  in
  List.rev (inner [])

let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | head :: tail -> Printf.printf "%s " (Token.string_of_token head); print_tokens tail
