open Types
open Printf

let alignment = 32

let find_var_index scope name =
  let rec loop ls acc =
  match ls with
  | [] -> raise Not_found
  | (head, _) :: tail ->
      if head = name then acc
      else loop tail (acc + 1) in
      loop (List.rev scope.vars) 0 

let generate_label () =
  let length = 16 in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    let random_letter = Char.chr (Char.code 'a' + Random.int 26) in
    Bytes.set buffer i random_letter done;
  buffer |> Bytes.to_string

let sp = "sp"
let t1 = "t1"
let t2 = "t2"
let a0 = "a0"

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

(*let mv dest src = [sprintf "mv %s, %s\n" dest src]*)
let push reg = addi sp sp (-alignment) @ [sprintf "sw %s, (sp)\n" reg]
let pop reg = [sprintf "lw %s, (sp)\n" reg] @ addi sp sp alignment
let lw_from_stack reg delta = [sprintf "lw %s, %d(sp)\n" reg delta]
let sw_to_stack reg delta = [sprintf "sw %s, %d(sp)\n" reg delta]
let call f = [sprintf "call %s\n" f]
let jump label = [sprintf "j %s\n" label]
let branch_true reg label = ["li t0, 1\n"; sprintf "beq %s, t0, %s\n" reg label]
let branch_false reg label = [sprintf "beqz %s, %s\n" reg label]

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
    | _ -> failwith "not implemented"

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
    | _ -> failwith "not implemented"
    in ll_print scope rest acc

and ll_printn scope exprs acc =
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
      @ call "printn_number"
    | _ -> failwith "not implemented"
    in ll_print scope rest acc

let assembly_of_typed_program typed_program =
  let instructions, _ = compile_program typed_program 0 [] in
  String.concat String.empty instructions
