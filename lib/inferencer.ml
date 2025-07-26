open Types

let rec find_var name = function
    | [] -> Error Was_Not_declared
    | head :: tail -> 
        match head with
        | (k, _) when k = name -> Ok (head)
        | _ -> find_var name tail

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

let init_scope = { vars = []; funcs = []; }

let rec specify_program_types scope program acc =
    match program with
    | [] -> Ok (List.rev acc)
    | statement :: rest ->
        match statement with
        | Declaration names ->
            (match infer_declaration scope names with
            | Error err -> Error err
            | Ok (new_scope, declaration) -> specify_program_types new_scope rest (declaration :: acc))
        | Assignment (name, expr) ->
            (match infer_assignment scope name expr with
            | Error err -> Error err
            | Ok (new_scope, assignment) -> specify_program_types new_scope rest (assignment :: acc))
        | While (condition, program) ->
            (match infer_while scope condition program with
            | Error err -> Error err
            | Ok (new_scope, typed_while) -> specify_program_types new_scope rest (typed_while :: acc))
        | _ -> failwith "not implemented"

and infer_declaration scope vars =
    if List.exists (fun var -> List.mem_assoc var scope.vars) vars
    then Error Already_declared (* TODO: show which exactly variable was declared *)
    else
        let typed_vars = List.map (fun var -> var, TNull) vars in
        let new_scope = { vars = scope.vars @ typed_vars; funcs = scope.funcs } in
        Ok (new_scope, (Typed_Declaration vars, scope))

and infer_assignment scope name expr =
    match infer_expression scope expr with
    | Error err -> Error err
    | Ok expr_type ->
    match List.assoc_opt name scope.vars with
    | None -> Error Was_Not_declared
    | Some var_type ->
        if var_type = TNull || var_type = expr_type then
            let vars =  (name, expr_type) :: (List.remove_assoc name scope.vars) in
            let new_scope = {vars = vars; funcs = scope.funcs} in
            Ok(new_scope, (Typed_Assignment(name, (expr, expr_type)), scope))
        else Error Expression_type_dismatch

and infer_while scope condition program =
    match infer_expression scope condition with
    | Error err -> Error err
    | Ok expr_type when expr_type = TBool ->
        (match specify_program_types scope program [] with
        | Error err -> Error err
        | Ok typed_program -> Ok (scope, (Typed_While((condition, TBool), typed_program), scope)))
    | _ -> Error Expression_type_dismatch

let infer_types program = specify_program_types init_scope program []
