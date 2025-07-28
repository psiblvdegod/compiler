open Types
open Printf
[@@@ocaml.warning "-32"]
let alignment = 32

let sp = "sp"
let t0 = "t0"
let t1 = "t1"
let t2 = "t2"
let s1 = "s1"
let a0 = "a0"
let a1 = "a1"
let a2 = "a2"
let a3 = "a3"
let a4 = "a4"
let a5 = "a5"
let a7 = "a7"
let zero = "x0"
let ecall = ["ecall\n"]
let sys_write = 64
let addi dest src lit = [sprintf "addi %s, %s, %d\n" dest src lit ]
let li reg lit = [sprintf "li %s, %d\n" reg lit]
let add dest left right = [sprintf "add %s, %s, %s\n" dest left right]
let sub dest left right = [sprintf "sub %s, %s, %s\n" dest left right]
let mul dest left right = [sprintf "mul %s, %s, %s\n" dest left right]
let div dest left right = [sprintf "div %s, %s, %s\n" dest left right]
let slt dest left right = [sprintf "slt %s, %s, %s\n" dest left right]
let sgt dest left right = [sprintf "slt %s, %s, %s\n" dest right left]
let sle dest left right = [sprintf "slt %s, %s, %s\n" dest right left; sprintf "seqz %s, %s\n" dest dest]
let sge dest left right = [sprintf "slt %s, %s, %s\n" dest left right; sprintf "seqz %s, %s\n" dest dest]
let seq dest left right = [sprintf "xor %s, %s, %s\n" dest left right; sprintf "seqz %s, %s\n" dest dest]
let sne dest left right = [sprintf "xor %s, %s, %s\n" dest left right; sprintf "snez %s, %s\n" dest dest]
let push reg = addi sp sp (-alignment) @ [sprintf "sw %s, (sp)\n" reg]
let pop reg = [sprintf "lw %s, (sp)\n" reg] @ addi sp sp alignment
let lw_from_stack reg delta = [sprintf "lw %s, %d(sp)\n" reg delta]
let ld_from_stack reg delta = [sprintf "ld %s, %d(sp)\n" reg delta]
let sw_to_stack reg delta = [sprintf "sw %s, %d(sp)\n" reg delta]
let sd_to_stack reg delta = [sprintf "sd %s, %d(sp)\n" reg delta]
let call f = [sprintf "call %s\n" f]
let jump label = [sprintf "j %s\n" label]
let branch_true reg label = ["li t0, 1\n"; sprintf "beq %s, t0, %s\n" reg label]
let branch_false reg label = [sprintf "beqz %s, %s\n" reg label]
let mv dest src = [sprintf "mv %s, %s\n" dest src]
let lb dest src = [sprintf "lb %s, (%s)\n" dest src]

let mmap_page = []
    @ li a0 0
    @ li a1 4096
    @ li a2 0x3
    @ li a3 0x22
    @ li a4 (-1)
    @ li a5 0
    @ li a7 222
    @ ecall

let generate_label () =
  let length = 16 in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    let random_letter = Char.chr (Char.code 'a' + Random.int 26) in
    Bytes.set buffer i random_letter done;
  buffer |> Bytes.to_string

let find_var_index scope name =
  let rec loop ls acc =
  match ls with
  | [] -> raise Not_found
  | (head, _) :: tail ->
      if head = name then acc
      else loop tail (acc + 1) in
      loop (List.rev scope.vars) 0 

let store_str dest str =
    let ls = String.to_seq str |> List.of_seq in
    let add ch = []
    @ li t0 (Char.code ch)
    @ ["sb t0, 0(t1)\n"]
    @ addi t1 t1 1 in
    let init = mv t1 dest in List.fold_left (fun acc ch -> acc @ add ch) init ls

(* source <- a0 *)
(* result -> a0 *)
let str_len_asciz =
  let loop_label = generate_label () in
  let end_loop_label = generate_label () in []
    @ mv a1 a0
    @ li a0 0
    @ [loop_label ^ ":\n"]
    @ lb t0 a1
    @ seq t1 t0 zero
    @ branch_true t1 end_loop_label
    @ addi a1 a1 1
    @ addi a0 a0 1
    @ jump loop_label
    @ [end_loop_label ^ ":\n"]

(* null-terminated *)
let create_str str = []
    @ mmap_page
    @ store_str a0 (str ^ "\x00")

let apply_binop = function
  | Add -> add t1 t1 t2
  | Sub -> sub t1 t1 t2
  | Mul -> mul t1 t1 t2
  | Div -> div t1 t1 t2
  
  | Lt  -> slt t1 t1 t2
  | Gt  -> sgt t1 t1 t2
  | Eq  -> seq t1 t1 t2
  | Leq -> sle t1 t1 t2
  | Geq -> sge t1 t1 t2
  | Neq -> sne t1 t1 t2
  | _ -> failwith "not implemented"

let apply_unop = function
  | Neg -> li t2 (-1) @ mul t1 t1 t2
  | _ -> failwith "not implemented"

let rec compile_expression scope expression temps acc =
    match expression with
    | Type_Int (Typed_value n) ->
      let acc = acc
      @ li t1 n
      @ push t1
      in acc

    | Type_Bool (Typed_value n) ->
      let acc = acc
      @ li t1 (if n = true then 1 else 0)
      @ push t1
      in acc

    | Type_Str (Typed_value s) ->
      let acc = acc
      @ create_str s
      @ addi sp sp (-alignment)
      @ ["sd a0, (sp)\n"]
      in acc
   
(* ===================================================================== *)
(*                           TODO : generalize                           *)

    | Type_Str (Typed_var name) ->
      let pos = (find_var_index scope name + temps) * alignment in      
      let acc = acc
      @ ld_from_stack t1 pos
      @ sd_to_stack t1 0
      in acc

    | Type_Str (Typed_unop (unop, typed_expr)) ->
      let acc = acc
      @ compile_expressions scope [typed_expr] temps
      @ ld_from_stack t1 0
      @ apply_unop unop
      @ sd_to_stack t1 0
      in acc

    | Type_Str (Typed_binop (binop, typed_left, typed_right)) ->
      let acc = acc
      @ compile_expressions scope [typed_left; typed_right] temps
      @ ld_from_stack t1 alignment
      @ ld_from_stack t2 0
      @ apply_binop binop
      @ addi sp sp alignment
      @ sd_to_stack t1 0
      in acc

(* ===================================================================== *)

    | Type_Bool (Typed_var name)
    | Type_Int (Typed_var name) ->
      let pos = (find_var_index scope name + temps) * alignment in      
      let acc = acc
      @ lw_from_stack t1 pos
      @ push t1
      in acc

    | Type_Bool (Typed_unop (unop, typed_expr))
    | Type_Int (Typed_unop (unop, typed_expr)) ->
      let acc = acc
      @ compile_expressions scope [typed_expr] temps
      @ lw_from_stack t1 0
      @ apply_unop unop
      @ sw_to_stack t1 0
      in acc
    
    | Type_Bool (Typed_binop (binop, typed_left, typed_right))
    | Type_Int (Typed_binop (binop, typed_left, typed_right)) ->
      let acc = acc
      @ compile_expressions scope [typed_left; typed_right] temps
      @ lw_from_stack t1 alignment
      @ lw_from_stack t2 0
      @ apply_binop binop
      @ addi sp sp alignment
      @ sw_to_stack t1 0
      in acc

and compile_expressions scope expressions position =
    let rec loop expressions var_amount code_acc =
      match expressions with
      | [] -> code_acc
      | expr :: rest ->
      let code_acc = compile_expression scope expr var_amount code_acc in
      loop rest (var_amount + 1) code_acc in

    loop expressions position []

let compile_expressions scope expressions = compile_expressions scope expressions 0

let rec compile_program typed_program local_cnt acc =
  match typed_program with
  | [] -> acc, local_cnt
  | (typed_statement, scope) :: rest ->
    
    match typed_statement with

    | Typed_Declaration names ->
      let local_cnt = local_cnt + (List.length names) in
      let delta = -(List.length names) * alignment in
      let acc = acc
      @ addi sp sp delta
      in compile_program rest local_cnt acc

    | Typed_Assignment(name, typed_expr) ->
      let pos = find_var_index scope name * alignment in
      let acc = acc
      @ compile_expressions scope [typed_expr]
      @ pop t1
      @ sw_to_stack t1 pos
      in compile_program rest local_cnt acc
    
    | Typed_While(condition, body_program) ->
      let while_label = generate_label () in
      let do_label = generate_label () in
      let done_label = generate_label () in

      let compiled_body, local_cnt' = compile_program body_program local_cnt [] in
      let wipe_locals = addi sp sp ((local_cnt' - local_cnt) * alignment) in

      let acc = acc
      @ [while_label ^ ":\n"] 
      @ compile_expressions scope [condition] 
      @ pop t1
      @ branch_true t1 do_label
      @ branch_false t1 done_label
      @ [do_label ^ ":\n"]
      @ compiled_body
      @ wipe_locals
      @ jump while_label
      @ [done_label ^ ":\n"]
      in compile_program rest local_cnt acc

    | Typed_Ite(condition, then_program, else_program) ->
      let then_label = generate_label () in
      let else_label = generate_label () in
      let fi_label = generate_label () in

      let compiled_then, locals_cnt_then = compile_program then_program local_cnt [] in
      let compiled_else, locals_cnt_else = compile_program else_program local_cnt [] in
      let wipe_locals_then = addi sp sp ((locals_cnt_then - local_cnt) * alignment) in
      let wipe_locals_else = addi sp sp ((locals_cnt_else - local_cnt) * alignment) in

      let acc = acc
      @ compile_expressions scope [condition] 
      @ pop t1
      @ branch_true t1 then_label
      @ branch_false t1 else_label
      @ [then_label ^ ":\n"]
      @ compiled_then
      @ wipe_locals_then
      @ jump fi_label
      @ [else_label ^ ":\n"]
      @ compiled_else
      @ wipe_locals_else
      @ [fi_label ^ ":\n"]
      in compile_program rest local_cnt acc

    | Typed_Call(name, args) ->
      let acc = acc @ (compile_call scope name args) in compile_program rest local_cnt acc
    | _ -> failwith "not implemented"

(* stdlib *)
and compile_call scope name args =
  match name with
  | "print" -> ll_print scope args []
  | "printn" -> ll_printn scope args []
  | _ -> failwith "not implemented"

and ll_print scope exprs acc =
  match exprs with
  | [] -> acc
  | expr :: rest ->
    let acc = acc @ compile_expressions scope [expr] in
    let acc = 
    match expr with
    | Type_Bool _ (* TODO : make prettier *)
    | Type_Int _ ->
      acc
      @ pop a0
      @ call "print_number"
    | Type_Str _ ->
      acc
      @ ld_from_stack a0 0
      @ addi sp sp alignment
      @ mv s1 a0
      @ str_len_asciz
      @ print_bytes_from s1 a0
      
    in ll_print scope rest acc

and ll_printn scope exprs acc =
  match exprs with
  | [] -> acc
  | expr :: rest ->
    let acc = ll_print scope [expr] acc in
    let acc = acc @ print_ch_imm '\n' in
    ll_printn scope rest acc

and print_bytes_from src len = []
    @ mv t1 src
    @ mv t2 len
    @ li a0 1
    @ mv a1 t1
    @ mv a2 t2
    @ li a7 sys_write
    @ ecall

and print_ch_imm ch = []
    @ addi sp sp (-1)
    @ li t0 (Char.code ch)
    @ ["sb t0, (sp)\n"]
    @ li a0 1
    @ mv a1 sp
    @ li a2 1
    @ li a7 sys_write
    @ ecall
    @ addi sp sp 1

let assembly_of_typed_program typed_program =
  let instructions, _ = compile_program typed_program 0 [] in
  String.concat String.empty instructions
