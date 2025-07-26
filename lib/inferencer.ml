open Types

let rec find_var name = function
    | [] -> Error Was_Not_declared
    | head :: tail -> 
        match head with
        | (k, _) when k = name -> Ok (head)
        | _ -> find_var name tail

let specify_var_type state var new_var_type =
    match List.assq_opt var state.vars with
    | None -> Error Was_Not_declared
    | Some old_var_type ->
        match old_var_type with
        | TNull ->
            let ls = List.remove_assq var state.vars in
            let state' = { vars = (var, new_var_type) :: ls; funcs = state.funcs } in Ok (state')
        | _     -> Error Already_specified

let type_of_binop = function
    | Eq
    | Neq
    | Leq
    | Geq
    | Lt
    | Gt
    | And
    | Or -> TBool
    
    | Mul
    | Div
    | Add
    | Sub -> TInt

    | Cat -> TStr

let type_of_unop = function
    | Neg -> TInt
    | Not -> TBool
    | Rev -> TStr

(* Error if type is TNull*)
let rec infer_expression scope = function
    | Int _  -> Ok TInt
    | Bool _ -> Ok TBool
    | Str _  -> Ok TStr

    | BinOp (binop, left, right) -> match_binop_type scope binop left right

    | UnOp (unop, arg) -> match_unop_type scope unop arg

    | Var id ->
        (match find_var id scope.vars with
        | Error err -> Error err
        | Ok (_, var_type) -> Ok(var_type))

and match_binop_type state binop left right =
    let match_operands_with_same_type expected_operands_type return_type =
        (match infer_expression state left with
        | Error err -> Error err
        | Ok (left_type) ->
            match infer_expression state right with
            | Error err -> Error err
            | Ok (right_type) ->
                if left_type = expected_operands_type && right_type = expected_operands_type
                then Ok return_type
                else Error Operand_type_dismatch) in

    match binop with
    | Eq | Neq | Leq | Geq | Lt | Gt -> match_operands_with_same_type TInt TBool
    | Add | Sub | Div | Mul -> match_operands_with_same_type TInt TInt
    | And | Or -> match_operands_with_same_type TBool TBool
    | Cat -> match_operands_with_same_type TStr TStr

and match_unop_type state unop operand =
    let match_operand expected_operand_type return_type =
        (match infer_expression state operand with
        | Error err -> Error err
        | Ok (operand_type) ->
            if operand_type = expected_operand_type
            then Ok return_type
            else Error Operand_type_dismatch) in
    match unop with
    | Neg -> match_operand TInt TInt
    | Rev -> match_operand TStr TStr
    | Not -> match_operand TBool TBool
    
let infer_assignment scope id expr =    
    match find_var id scope.vars with
    | Error err -> Error err
    | Ok(var, var_type) ->
            match infer_expression scope expr with
            | Error err -> Error err
            | Ok type_of_expr ->
                match var_type with
                | TNull -> specify_var_type scope var type_of_expr
                | x when x = type_of_expr -> Ok(scope)
                | _ -> Error Already_specified

let init_scope = { vars = []; funcs = []; }

let rec specify_program_types scope program acc =
    match program with
    | [] -> Ok (List.rev acc)
    | statement :: rest ->
        match statement with
        | Declaration names ->
            (match infer_declaration scope names with
            | Error err -> Error err
            | Ok new_scope -> specify_program_types new_scope rest ((Declaration names, scope) :: acc))
        | Assignment (name, expr) ->
            (match infer_assignment scope name expr with
            | Error err -> Error err
            | Ok new_scope -> specify_program_types new_scope rest ((Assignment(name, expr), scope) :: acc))
        | _ -> failwith "not implemented"

and infer_declaration scope vars =
    if List.exists (fun var -> List.mem_assq var scope.vars) vars
    then Error Already_declared (* TODO: show which exactly variable was declared *)
    else
        let typed_vars = List.map (fun var -> var, TNull) vars in
        Ok ({ vars = scope.vars @ typed_vars; funcs = scope.funcs })

let infer_types program = specify_program_types init_scope program []
