open Token

let get_tokens_of_expression expression =
  let buffer = Lexing.from_string expression in
  let rec inner result =
    match token buffer with
    | EOF -> EOF :: result
    | x -> inner (x :: result)
  in
  List.rev (inner [])
