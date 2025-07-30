(* psiblvdegod, 2025, under MIT License *)

[@@@ocamlformat "disable"]

open Types
open Asm
open Asm.Reg
open Asm.Instr
open Asm.Str
open Asm.Print

let rec compile_expression scope expression temps acc =
  match expression with
  | Type_Int (Typed_value n) ->
      let acc = acc
      @ li t1 n
      @ addi sp sp (-alignment)
      @ sw_to_stack t1 0 in
      acc
  | Type_Bool (Typed_value n) ->
      let acc = acc
        @ li t1 (if n = true then 1 else 0)
        @ addi sp sp (-alignment)
        @ sw_to_stack t1 0
      in
      acc
  | Type_Str (Typed_value s) ->
      let acc = acc
        @ create_asciz s
        @ addi sp sp (-alignment)
        @ sd_to_stack a0 0
      in
      acc
  | Type_Str (Typed_var name) ->
      let pos = (find_var_index scope name + temps) * alignment in
      let acc = acc
      @ ld_from_stack t1 pos
      @ sd_to_stack t1 0 in
      acc
  | Type_Str (Typed_unop (unop, typed_expr)) ->
      let acc = acc
        @ compile_expressions scope [ typed_expr ] temps
        @ ld_from_stack t1 0
        @ apply_unop unop
        @ sd_to_stack t1 0
      in
      acc
  | Type_Str (Typed_binop (binop, typed_left, typed_right)) ->
      let acc = acc
        @ compile_expressions scope [ typed_left; typed_right ] temps
        @ ld_from_stack t1 alignment
        @ ld_from_stack t2 0
        @ apply_binop binop
        @ addi sp sp alignment
        @ sd_to_stack t1 0
      in
      acc
  | Type_Bool (Typed_var name) | Type_Int (Typed_var name) ->
      let pos = (find_var_index scope name + temps) * alignment in
      let acc = acc
        @ lw_from_stack t1 pos
        @ addi sp sp (-alignment)
        @ sw_to_stack t1 0
      in
      acc
  | Type_Bool (Typed_unop (unop, typed_expr))
  | Type_Int (Typed_unop (unop, typed_expr)) ->
      let acc = acc
        @ compile_expressions scope [ typed_expr ] temps
        @ lw_from_stack t1 0
        @ apply_unop unop
        @ sw_to_stack t1 0
      in
      acc
  | Type_Bool (Typed_binop (binop, typed_left, typed_right))
  | Type_Int (Typed_binop (binop, typed_left, typed_right)) ->
      let acc = acc
        @ compile_expressions scope [ typed_left; typed_right ] temps
        @ lw_from_stack t1 alignment
        @ lw_from_stack t2 0
        @ apply_binop binop
        @ addi sp sp alignment
        @ sw_to_stack t1 0
      in
      acc

and compile_expressions scope expressions position =
  let rec loop expressions var_amount code_acc =
    match expressions with
    | [] -> code_acc
    | expr :: rest ->
        let code_acc = compile_expression scope expr var_amount code_acc in
        loop rest (var_amount + 1) code_acc
  in

  loop expressions position []

and find_var_index scope name =
  let rec loop ls acc =
    match ls with
    | [] -> raise Not_found
    | (head, _) :: tail -> if head = name then acc else loop tail (acc + 1)
  in
  loop (List.rev scope.vars) 0

and apply_binop = function
  | Add -> add t1 t1 t2
  | Sub -> sub t1 t1 t2
  | Mul -> mul t1 t1 t2
  | Div -> div t1 t1 t2
  | Lt -> slt t1 t1 t2
  | Gt -> sgt t1 t1 t2
  | Eq -> seq t1 t1 t2
  | Leq -> sle t1 t1 t2
  | Geq -> sge t1 t1 t2
  | Neq -> sne t1 t1 t2
  | And -> mul t1 t1 t2
  | Or -> add t1 t1 t2 @ sge t1 t1 zero
  | _ -> raise Not_implemented

and apply_unop = function
  | Neg -> li t2 (-1) @ mul t1 t1 t2
  | Not -> seq t1 t1 zero
  | _ -> raise Not_implemented

let compile_expressions scope expressions =
  compile_expressions scope expressions 0

let rec compile_program typed_program local_cnt acc =
  match typed_program with
  | [] -> (acc, local_cnt)
  | (typed_statement, scope) :: rest -> (
      match typed_statement with
      | Typed_Declaration names ->
          let local_cnt = local_cnt + List.length names in
          let delta = -List.length names * alignment in
          let acc = acc @ addi sp sp delta in
          compile_program rest local_cnt acc
      | Typed_Assignment (name, typed_expr) ->
          let pos = find_var_index scope name * alignment in
          let acc =
            acc
            @ compile_expressions scope [ typed_expr ]
            @ lw_from_stack t1 0 @ addi sp sp alignment @ sw_to_stack t1 pos
          in
          compile_program rest local_cnt acc
      | Typed_While (condition, body_program) ->
          let while_label = generate_label () in
          let do_label = generate_label () in
          let done_label = generate_label () in

          let compiled_body, local_cnt' =
            compile_program body_program local_cnt []
          in
          let wipe_locals = addi sp sp ((local_cnt' - local_cnt) * alignment) in

          let acc =
            acc
            @ [ while_label ^ ":\n" ]
            @ compile_expressions scope [ condition ]
            @ lw_from_stack t1 0 @ addi sp sp alignment
            @ branch_true t1 do_label @ branch_false t1 done_label
            @ [ do_label ^ ":\n" ]
            @ compiled_body @ wipe_locals @ jump while_label
            @ [ done_label ^ ":\n" ]
          in
          compile_program rest local_cnt acc
      | Typed_Ite (condition, then_program, else_program) ->
          let then_label = generate_label () in
          let else_label = generate_label () in
          let fi_label = generate_label () in

          let compiled_then, locals_cnt_then =
            compile_program then_program local_cnt []
          in
          let compiled_else, locals_cnt_else =
            compile_program else_program local_cnt []
          in
          let wipe_locals_then =
            addi sp sp ((locals_cnt_then - local_cnt) * alignment)
          in
          let wipe_locals_else =
            addi sp sp ((locals_cnt_else - local_cnt) * alignment)
          in
          let acc =
            acc
            @ compile_expressions scope [ condition ]
            @ lw_from_stack t1 0
            @ addi sp sp alignment
            @ branch_true t1 then_label
            @ branch_false t1 else_label
            @ [ then_label ^ ":\n" ]
            @ compiled_then @ wipe_locals_then
            @ jump fi_label
            @ [ else_label ^ ":\n" ]
            @ compiled_else @ wipe_locals_else
            @ [ fi_label ^ ":\n" ]
          in
          compile_program rest local_cnt acc
      | Typed_Call (name, args) ->
          let acc = acc
            @ compile_call scope name args in
          compile_program rest local_cnt acc
      | Typed_Definition (name, typed_args, typed_program) ->
          let end_label = generate_label () in
          let acc = acc
            @ jump end_label
            @ [ name ^ ":\n" ] in
          let acc, local_cnt' =
            compile_program typed_program
              (local_cnt + List.length typed_args)
              acc
          in
          let acc =
            acc
            @ addi sp sp ((local_cnt' - local_cnt) * alignment)
            @ [ "ret\n" ]
            @ [ end_label ^ ":\n" ]
          in
          compile_program rest local_cnt acc)

and compile_call scope name args =
  match name with
  | "print" -> print scope args []
  | "printn" -> printn scope args []
  | name -> compile_expressions scope args @ [ Printf.sprintf "call %s\n" name ]

and print scope exprs acc =
  match exprs with
  | [] -> acc
  | expr :: rest ->
      let acc =
        acc
        @ compile_expressions scope [ expr ]
        @ lw_from_stack a0 0 @ addi sp sp alignment
      in
      let acc =
        match expr with
        | Type_Bool _ -> acc @ print_bool a0
        | Type_Int _ -> acc @ print_number a0
        | Type_Str _ -> acc @ mv s1 a0 @ str_len_asciz () @ print_bytes_from s1 a0
      in
      print scope rest acc

and printn scope exprs acc =
  match exprs with
  | [] -> acc
  | expr :: rest ->
      let acc = print scope [ expr ] acc in
      let acc = acc @ print_str_imm "\n" in
      printn scope rest acc

let assembly_of_typed_program typed_program =
  let instructions, _ = compile_program typed_program 0 [] in
  String.concat String.empty instructions
