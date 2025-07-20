open Types
open Printf

exception ID_is_already_occupied
exception Unbound_value
exception Not_supported

type generator_state =
{
  vars : string list;
  acc : string list;
}

let init_state =
{
  vars = [];
  acc = [];
}

let alignment = 16

let append_to_acc state str = { vars = state.vars; acc = str :: state.acc  }

let inc_sp state = sprintf "addi sp, sp, %d\n" (alignment) |>  append_to_acc state

let dec_sp state = sprintf "addi sp, sp, %d\n" (-alignment) |>  append_to_acc state

let index_of_var_or_raise state name =
    let rec loop value acc = function
        | [] -> raise Unbound_value
        | head :: rest ->
          if head = value
          then acc
          else loop value (acc + 1) rest in
          
    loop name 0 state.vars

let generate_label () =
  let length = 16 in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    let random_letter = Char.chr (Char.code 'a' + Random.int 26) in
    Bytes.set buffer i random_letter done;
  buffer |> Bytes.to_string

let mnemonic_of_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"

let rec parse_expressions state cnt = function
  | [] -> state
  
  | Int value :: rest ->
    let state = dec_sp state in
    let state = sprintf "li t1, %d\n" value |> append_to_acc state in
    let state = sprintf "sw t1, (sp)\n"     |> append_to_acc state in
    parse_expressions state (cnt + 1) rest
  
  | Var name :: rest ->
    let index = index_of_var_or_raise state name in
    let pos = (index + cnt) * alignment in
    let state = sprintf "lw t1, %d(sp)\n" pos |> append_to_acc state in
    let state = dec_sp state in
    let state = "sw t1, (sp)\n" |> append_to_acc state in    
    parse_expressions state (cnt + 1) rest

  | BinOp(binop, left, right) :: rest ->
      let mnemo = mnemonic_of_binop binop in
      let state = parse_expressions state cnt [left; right] in
      let state = sprintf "lw t1, %d(sp)\n" alignment |> append_to_acc state in
      let state = sprintf "lw t2, (sp)\n"             |> append_to_acc state in
      let state = inc_sp state in
      let state = sprintf "%s t1, t1, t2\n" mnemo     |> append_to_acc state in
      let state = sprintf "sw t1, (sp)\n"             |> append_to_acc state in
      parse_expressions state (cnt + 1) rest

  | Neg(expr) :: rest ->
    let state = parse_expressions state cnt [expr] in
    let state = "lw t1, (sp)\n"    |> append_to_acc state in
    let state = "li t2, -1\n"      |> append_to_acc state in
    let state = "mul t1, t1, t2\n" |> append_to_acc state in
    let state = "sw t1, (sp)\n"    |> append_to_acc state in
    parse_expressions state cnt rest

  | _ -> raise Not_supported

let parse_expressions state expressions = parse_expressions state 0 expressions

let mnemonic_of_comparison = function
    | Eq  -> "beq"
    | Neq -> "bne"
    | Lt  -> "blt"
    | Gt  -> "bgt"
    | Leq -> "ble"
    | Geq -> "bge"

let parse_condition state bool_expr then_label else_label =
    match bool_expr with
    | true -> sprintf "j %s\n" then_label  |> append_to_acc state
    | false -> sprintf "j %s\n" else_label |> append_to_acc state
    | Comparison(cmp, left, right) ->
      let mnemo = mnemonic_of_comparison cmp in
      let state = parse_expressions state [left; right] in
      let state = sprintf "lw t1, %d(sp)\n" alignment        |> append_to_acc state in
      let state = sprintf "lw t2, (sp)\n"                    |> append_to_acc state in
      let state = inc_sp(inc_sp state) in
      let state = sprintf "%s t1, t2, %s\n" mnemo then_label |> append_to_acc state in
      let state = sprintf "j %s\n" else_label                |> append_to_acc state in state

let rec process_program state = function
  | [] -> state
  | statement :: rest ->
  match statement with
  | Ite(condition, then_program, else_program) ->
      process_program (process_ite state condition then_program else_program) rest
  | Assignment(name,expression) ->
      process_program (process_assignment state name expression) rest
  | While(condition, program) ->
      process_program (process_while state condition program) rest
  | Declaration names ->
      process_program (process_declaration state names) rest
  | Call(name, args) ->
      process_program (process_calling state name args) rest

and process_ite state bool_expr then_program else_program =
    let then_label = generate_label () in
    let else_label = generate_label () in
    let fi_label = generate_label () in

    let state = parse_condition state bool_expr then_label else_label in
    let state = sprintf "%s:\n" then_label |>  append_to_acc state in
    let state = process_program state then_program in
    let state = sprintf "j %s\n" fi_label  |> append_to_acc state in
    let state = sprintf "%s:\n" else_label |>  append_to_acc state in
    let state = process_program state else_program in
    let state = sprintf "%s:\n" fi_label   |> append_to_acc state in state

and process_assignment state destination expression =
  let destination_index = index_of_var_or_raise state destination in
  let pos = alignment * destination_index in
  let state = parse_expressions state [expression] in
  let state = sprintf "lw t1, (sp)\n"               |> append_to_acc state in
  let state = sprintf "addi sp, sp, %d\n" alignment |> append_to_acc state in
  let state = sprintf "sw t1, %d(sp)\n" pos         |> append_to_acc state in state

and process_while state bool_expr statements =
    let while_label = generate_label () in
    let do_label = generate_label () in
    let done_label = generate_label () in
    let state = sprintf "%s:\n" while_label |> append_to_acc state in
    let state = parse_condition state bool_expr do_label done_label in
    let state = sprintf "%s:\n" do_label |>  append_to_acc state in
    let state = process_program state statements in
    let state = sprintf "j %s\n" while_label |> append_to_acc state in
    
    sprintf "%s:\n" done_label |> append_to_acc state

and process_declaration state = function
  | [] -> state
  | name :: rest ->
  if List.mem name state.vars then
      raise ID_is_already_occupied
  else
      let state = { vars = name :: state.vars; acc = state.acc; } in
      let state = dec_sp state in
      process_declaration state rest

and process_calling state name args =
  match name with
  | "print" -> lang_print state args
  | _ -> raise Not_supported

(* stdlib *)
and lang_print state = function
| [] -> raise (Invalid_argument "at least 1 argument expected.")
| args ->
  let rec loop cnt state =
    if cnt = 0 then state else
      let state = "lw a0, (sp)\n"       |> append_to_acc state in
      let state = inc_sp state in
      let state = "call print_number\n" |> append_to_acc state in

      loop (cnt - 1) state in
  
  parse_expressions state (List.rev args) |> loop (List.length args)

let assembly_of_program program =
  let strs = (process_program init_state program).acc in
  strs |> List.rev |> String.concat String.empty
