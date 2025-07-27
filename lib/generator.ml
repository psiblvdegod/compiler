open Types
open Printf

let alignment = 32

let find_var_index scope name =
  let rec loop ls acc =
  match ls with
  | [] -> raise Not_found
  | (head, _) :: tail -> if head = name then acc else loop tail (acc + 1) in loop (List.rev scope.vars) 0 

let sp = "sp"
let t1 = "t1"
let t2 = "t2"

let addi dest src lit = [sprintf "addi %s, %s, %d\n" dest src lit ]
let li reg lit = [sprintf "li %s, %d\n" reg lit]
let add dest left right = [sprintf "add %s, %s, %s\n" dest left right]
let sub dest left right = [sprintf "sub %s, %s, %s\n" dest left right]
let mul dest left right = [sprintf "mul %s, %s, %s\n" dest left right]
let div dest left right = [sprintf "div %s, %s, %s\n" dest left right]

let push reg = addi sp sp (-alignment) @ [sprintf "sw %s, (sp)\n" reg]
let pop reg = [sprintf "lw %s, (sp)\n" reg] @ addi sp sp alignment
let lw_from_stack reg delta = [sprintf "lw %s, %d(sp)\n" reg delta]
let sw_to_stack reg delta = [sprintf "sw %s, %d(sp)\n" reg delta]

let apply_binop = function
  | Add -> add t1 t1 t2
  | Sub -> sub t1 t1 t2
  | Mul -> mul t1 t1 t2
  | Div -> div t1 t1 t2
  | _ -> failwith "not implemented"

let apply_unop = function
  | Neg -> li t2 (-1) @ mul t1 t1 t2
  | _ -> failwith "not implemented"

let rec compile_expressions scope expressions temps acc =
  match expressions with
  | [] -> acc, temps
  | Type_Int (Typed_value n) :: rest ->
    let acc = acc
    @ li t1 n
    @ push t1
    in compile_expressions scope rest (temps + 1) acc

  | Type_Int (Typed_var name) :: rest ->
    let pos = (find_var_index scope name + temps) * alignment in
    let acc = acc
    @ lw_from_stack t1 pos
    @ push t1
    in compile_expressions scope rest (temps + 1) acc

  | Type_Int (Typed_unop (unop, typed_expr)) :: rest ->
    let (compile_operand, temps2) = compile_expressions scope [typed_expr] temps [] in
    let delta = (temps2 - temps) * alignment in
    let acc = acc
    @ compile_operand
    @ lw_from_stack t1 0
    @ addi sp sp delta
    @ apply_unop unop
    @ push t1
    in compile_expressions scope rest (temps + 1) acc
  
  | Type_Int (Typed_binop (binop, typed_left, typed_right)) :: rest ->
    let (compile_left, temps2) = compile_expressions scope [typed_left] temps [] in
    let (compile_right, temps3) = compile_expressions scope [typed_right] temps2 [] in
    let delta1 = (temps3 - temps2) * alignment in
    let delta2 = (temps3 - temps) * alignment in
    let acc = acc
    @ compile_left
    @ compile_right
    @ lw_from_stack t1 delta1 (* load left operand *)
    @ lw_from_stack t2 0      (* load right operand *)
    @ addi sp sp delta2       (* wipe stack *)
    @ apply_binop binop
    @ push t1
    in compile_expressions scope rest (temps + 1) acc
  | _ -> failwith "not implemented"

let compile_expressions scope expressions =
  let (res, _) = compile_expressions scope expressions 0 [] in res

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
    | Typed_Assignment (name, typed_expr) ->
      let pos = find_var_index scope name * alignment in
      let acc = acc
      @ compile_expressions scope [typed_expr]
      @ pop t1
      @ sw_to_stack t1 pos
      in compile_program rest acc
    | _ -> failwith "not implemented"

let assembly_of_typed_program typed_program = String.concat String.empty (compile_program typed_program [])
