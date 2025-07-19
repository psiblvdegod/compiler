open Types
open Printf

exception ReDeclaration_attempt
exception Unbound_value
exception Not_supported

type generator_state =
{
  vars : string list;
  acc : string;
  sp : int;
}

let init_state =
{
  vars = [];
  sp = 0;
  acc = String.empty
}

let alignment = 16

let append_to_acc state str = { vars = state.vars; sp = state.sp; acc = state.acc ^ str }

let rec process_declaration state = function
  | [] -> state
  | name :: rest ->
  if List.mem name state.vars
  then raise ReDeclaration_attempt
  else let new_state =
  {
    vars = name :: state.vars;
    sp = state.sp + alignment;
    acc = state.acc ^ (sprintf "addi sp, sp, %d\n" (-alignment));
  } in process_declaration new_state rest

let index_of_var_or_raise state name =
    let rec loop value acc = function
        | [] -> raise Unbound_value
        | head :: rest ->
          if head = value
          then acc
          else loop value (acc + 1) rest in
          
    loop name 0 state.vars

let push value =
    sprintf
"addi sp, sp, %d
li t1, %d
sw t1, 0(sp)\n" (-alignment) value

let generate_label () =
  let length = 16 in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    let random_letter = Char.chr (Char.code 'a' + Random.int 26) in
    Bytes.set buffer i random_letter done;
  buffer |> Bytes.to_string

let apply_binop mnemonic =
sprintf
"
lw t2, (sp)
lw t1, %d(sp)
addi sp, sp, %d

\n%s t1, t1, t2
sw t1, (sp)\n
" alignment alignment mnemonic

let mnemonic_of_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"

let rec parse_expressions state cnt = function
  | [] -> state
  | Int value :: rest -> parse_expressions (push value |> append_to_acc state) (cnt + 1) rest
  | Var name :: rest ->
    let index = index_of_var_or_raise state name in
    let copy =
        sprintf "lw t1, %d(sp)\n" ((index + cnt) * alignment) ^
        sprintf "addi sp, sp, %d\n" (-alignment) ^
        sprintf "sw t1, (sp)\n" in
    parse_expressions (append_to_acc state copy) (cnt + 1) rest
  | BinOp(binop, left, right) :: rest ->
    let parse_operands = parse_expressions state cnt [left; right] in
    let new_state = append_to_acc parse_operands (apply_binop @@ mnemonic_of_binop binop) in
    parse_expressions new_state (cnt + 1) rest
  | _ -> raise Not_supported

let parse_expressions state expressions = parse_expressions state 0 expressions

let mnemonic_of_comparison = function
    | Eq -> "beq"
    | Neq -> "bne"
    | Lt -> "blt"
    | Gt -> "bgt"
    | Leq -> "ble"
    | Geq -> "bge"

let parse_condition state bool_expr then_label else_label =
    match bool_expr with
    | true -> sprintf "j %s\n" then_label |> append_to_acc state
    | false -> sprintf "j %s\n" else_label |> append_to_acc state
    | Comparison(cmp, left, right) ->
      let state = parse_expressions state [left; right] in
      
      sprintf "lw t1, %d(sp)\n" alignment ^
      sprintf "lw t2, (sp)\n" ^
      sprintf "addi sp, sp, %d\n" (2 * alignment) ^
      sprintf "%s t1, t2, %s\n" (mnemonic_of_comparison cmp) then_label ^
      sprintf "j %s\n" else_label
      
      |> append_to_acc state

let lang_print state = function
| [] -> Invalid_argument "at least 1 argument expected." |> raise
| args ->
  let rec loop cnt state =
    if cnt = 0 then state else
      let pop = "lw a0, (sp)\n" ^ sprintf "addi sp, sp, %d\n" alignment in
      pop ^ "call print_number\n" |> append_to_acc state |> loop (cnt - 1) in
  
  parse_expressions state (List.rev args) |> loop (List.length args)

let process_calling state name args =
  match name with
  | "print" -> lang_print state args
  | _ -> raise Not_supported

let process_assignment state destination expression =
  let state = parse_expressions state [expression] in
  let destination_index = index_of_var_or_raise state destination in
  let pop = "lw t1, (sp)\n" ^ sprintf "addi sp, sp, %d\n" alignment in
  let allign = sprintf "sw t1, %d(sp)\n" (alignment * destination_index) in

  append_to_acc state (pop ^ allign)

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
    

and process_ite state bool_expr then_program else_program =
    let then_label = generate_label () in
    let else_label = generate_label () in
    let fi_label = generate_label () in

    let state = parse_condition state bool_expr then_label else_label in
    let state = sprintf "%s:\n" then_label |>  append_to_acc state in
    let state = process_program state then_program in
    let state = sprintf "j %s\n" fi_label |> append_to_acc state in
    let state = sprintf "%s:\n" else_label |>  append_to_acc state in
    let state = process_program state else_program in

    sprintf "%s:\n" fi_label |> append_to_acc state

let assembly_of_program program = (process_program init_state program).acc
