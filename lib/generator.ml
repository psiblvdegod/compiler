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
(*let mv dest src = [sprintf "mv %s, %s\n" dest src]*)
let push reg = addi sp sp (-alignment) @ [sprintf "sw %s, (sp)\n" reg]
let pop reg = [sprintf "lw %s, (sp)\n" reg] @ addi sp sp alignment
let lw_from_stack reg delta = [sprintf "lw %s, %d(sp)\n" reg delta]
let sw_to_stack reg delta = [sprintf "sw %s, %d(sp)\n" reg delta]
let call f = [sprintf "call %s\n" f]
let print_number = "print_number"

let apply_binop = function
  | Add -> add t1 t1 t2
  | Sub -> sub t1 t1 t2
  | Mul -> mul t1 t1 t2
  | Div -> div t1 t1 t2
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

    | Type_Int (Typed_var name) ->
      let pos = (find_var_index scope name + temps) * alignment in      
      let acc = acc
      @ lw_from_stack t1 pos
      @ push t1
      in acc

    | Type_Int (Typed_unop (unop, typed_expr))->
      let acc = acc
      @ compile_expressions scope [typed_expr] temps
      @ lw_from_stack t1 0
      @ apply_unop unop
      @ sw_to_stack t1 0
      in acc
    
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
      let code_acc = match expr with
      | Type_Int _ | Type_Bool _ -> code_acc
      | Type_Str _ -> failwith "not implemented"
      in loop rest (var_amount + 1) code_acc in

    loop expressions position []

let compile_expressions scope expressions = compile_expressions scope expressions 0

let rec compile_program typed_program acc =
  match typed_program with
  | [] -> acc
  | (typed_statement, scope) :: rest ->
    match typed_statement with
    | Typed_Declaration names ->
      let delta = -(List.length names) * alignment in
      let acc = acc
      @ addi sp sp delta
    in compile_program rest acc
    | Typed_Assignment(name, typed_expr) ->
      let pos = find_var_index scope name * alignment in
      let acc = acc
      @ compile_expressions scope [typed_expr]
      @ pop t1
      @ sw_to_stack t1 pos
      in compile_program rest acc

    | Typed_Call(name, args) ->
      let acc = acc @ (compile_call scope name args) in
      compile_program rest acc
    | _ -> failwith "not implemented"

(* stdlib *)
and compile_call scope name args =
  match name with
  | "print" -> ll_print scope args []
  | _ -> failwith "not implemented"

and ll_print scope exprs acc =
  match exprs with
  | [] -> acc
  | expr :: rest ->
    let acc = acc @ compile_expressions scope [expr] in
    let acc = 
    match expr with
    | Type_Int _ ->
      acc
      @ pop a0
      @ call print_number
    | _ -> failwith "not implemented"
    in ll_print scope rest acc

let assembly_of_typed_program typed_program = String.concat String.empty (compile_program typed_program [])
