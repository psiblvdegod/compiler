open Types

let rec find_var id = function
    | [] -> Error Was_Not_declared
    | head :: tail -> 
        match head with
        | (k, _) when k = id -> Ok (head)
        | _ -> find_var id tail

let specify_var_type state var new_var_type =
    match List.assq_opt var state.vars with
    | None -> Error Was_Not_declared
    | Some old_var_type ->
        match old_var_type with
        | TNull ->
            let ls = List.remove_assq var state.vars in
            let state' = { vars = (var, new_var_type) :: ls; funcs = state.funcs } in Ok (state')
        | _     -> Error (Already_specified)

(* TODO: show which exactly variable was declared *)
let infer_declaration state vars =
    if List.exists (fun var -> List.mem_assq var state.vars) vars
    then Error Already_declared
    else
        let init_types ls = List.map (fun var -> var, TNull) ls in
        Ok ({ vars = state.vars @ (init_types vars); funcs = state.funcs })

let type_of_binop = function
    | Eq
    | Neq
    | Leq
    | Geq
    | Lt
    | Gt
    | And    (* TODO : change type to bool (somehow distinguish return type and operands type)*)

    | Mul
    | Div
    | Add
    | Sub -> TInt
    
    | Or -> TBool

    | Cat -> TStr

let type_of_unop = function
    | Neg -> TInt
    | Not -> TBool
    | Rev -> TStr

(* Error if type is TNull*)
let rec infer_expression state = function
    | Int _  -> Ok TInt
    | Bool _ -> Ok TBool
    | Str _  -> Ok TStr

    | BinOp (binop, left, right) -> match_binop_type state binop left right

    | UnOp (unop, arg) -> match_unop_type state unop arg

    | Var id ->
        (match find_var id state.vars with
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

let infer_assignment state id expr =    
    match find_var id state.vars with
    | Error err -> Error err
    | Ok(var, var_type) ->
        let type_of_expr_res = infer_expression state expr in
            match type_of_expr_res with
            | Error err -> Error err
            | Ok type_of_expr ->
                match var_type with
                | TNull -> specify_var_type state var type_of_expr
                | x when x = type_of_expr -> Ok(state)
                | _ -> Error Already_specified

let init_state =
    { vars = []; funcs = []; }

let rec type_check_program state = function
    | [] -> Ok (state)
    | statement :: rest ->
    (
        let check = function
        | Declaration vars -> infer_declaration state vars
        | Assignment (var, expr) -> infer_assignment state var expr
        | _ -> failwith "Not implemented"

        in match check statement with
        | Error err -> Error err
        | Ok state -> type_check_program state rest
    )

let infer_types program = type_check_program init_state program 
