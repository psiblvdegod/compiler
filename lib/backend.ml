open Types
open Printf

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
  sprintf "li t1, %d\nsw t1, %d(sp)\n" value (index * 16)

let lw_t1_by_index index =
    sprintf "lw t1, %d(sp)\n" (index * 16)

let sw_t1_by_index index =
    sprintf "sw t1, %d(sp)\n" (index * 16)

let push value =
    sprintf
"addi sp, sp, -16
li t1, %d
sw t1, 0(sp)\n" value

let rec parse_binop state cnt left right binop =
  let calc_left, left_cnt = parse_expression state cnt left in
  let calc_right, right_cnt = parse_expression state left_cnt right in
  let lw_left = sprintf "lw t1, %d(sp)\n" (16 * (right_cnt - left_cnt)) in
  let lw_right = "lw t2, 0(sp)\n" in
  let calc_res = sprintf "%s t1, t1, t2\n" binop in
  let wipe_stack = sprintf "addi sp, sp, %d\n" (16 * (right_cnt - cnt - 1)) in
  let push_res = "sw t1, 0(sp)\n" in
  calc_left ^ calc_right ^ lw_left ^ lw_right ^ calc_res ^ wipe_stack ^ push_res, right_cnt - cnt

and parse_expression state cnt = function
  | Int value -> push value, cnt + 1
  | Var name ->
    let index = index_of_var state name in
    lw_t1_by_index (index + cnt) ^ "addi sp, sp, -16\n" ^ "sw t1, 0(sp)\n", cnt + 1
  | Add(left, right) -> parse_binop state cnt left right "add"
  | Sub(left, right) -> parse_binop state cnt left right "sub"
  | Mul(left, right) -> parse_binop state cnt left right "mul"
  | Div(left, right) -> parse_binop state cnt left right "div"
  | _ -> raise Unsupported

let process_assignment state dest expression =
  let dest_index = index_of_var state dest in
  let expr, _ = parse_expression state 0 expression in
  (* if rest != 1 then failwith ";(" else *)
  expr ^ "lw t1, 0(sp)\n" ^ "addi sp, sp, 16\n" ^ (sprintf "sw t1, %d(sp)\n" (16 * dest_index))
  |> append_to_acc state

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
  acc = String.empty
}

let assembly_of_program program = assembly_of_program init_state program
