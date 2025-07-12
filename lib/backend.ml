open Types

exception ReDeclaration_attempt
exception Unbound_value
exception Unsupported

type generator_state =
{
  vars : string list;
  acc : string;
  sp : int;
}

let declaration_code = "addi sp, sp, -16\n"

let append_to_acc state str = { vars = state.vars; sp = state.sp; acc = state.acc ^ str }

let rec process_declaration state = function
  | [] -> state
  | name :: rest ->
  if List.mem name state.vars
  then raise ReDeclaration_attempt
  else let new_state =
  {
    vars = name :: state.vars;
    sp = state.sp + 16;
    acc = state.acc ^ declaration_code;
  } in process_declaration new_state rest

let index_of_var state name =
  match List.find_index (fun x -> x = name) state.vars with
  | None -> raise Unbound_value
  | Some index -> index

let assignment_const index value = 
  Printf.sprintf "li t1, %d\nsw t1, %d(sp)\n" value (index * 16)

let lw_t1_by_index index =
    Printf.sprintf "lw t1, %d(sp)\n" (index * 16)

let sw_t1_by_index index =
    Printf.sprintf "sw t1, %d(sp)\n" (index * 16)

let process_assignment state dest expression =
  let dest_index = index_of_var state dest in
  let get_str = function
  | Int value -> assignment_const dest_index value
  | Var source ->
    (index_of_var state source |> lw_t1_by_index) ^ (sw_t1_by_index dest_index)
  | _ -> raise Unsupported in
  append_to_acc state (get_str expression)

let rec assembly_of_program state = function
  | [] -> state.acc
  | statement :: rest ->
  match statement with
  | Declaration names ->
    let new_state = process_declaration state names in
    assembly_of_program new_state rest
  | Assignment(name,expression) ->
    let new_state = process_assignment state name expression in
    assembly_of_program new_state rest
  | _ -> raise Unsupported

let init_state =
  {
    vars = [];
    sp = 0;
    acc =
"
.global _start
.section .text
    
_start:
"
  }

let assembly_of_program program = assembly_of_program init_state program
