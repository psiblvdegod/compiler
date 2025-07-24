open Types
open Printf

type error =
    | Already_declared
    | Was_Not_declared
    | Was_Not_defined
    | Was_Not_assigned
    | Operand_type_dismatch
    | Function_type_dismatch
    | Already_specified (* TODO : rename *)

type expr_type = | TInt | TBool | TStr | TNull

and state = 
{
    vars : (id * expr_type) list;
    funcs: (id * expr_type * (expr_type list)) list;
}

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

(* TODO : get all errors with info of invalid arguments *)
let rec unfold_res acc = function
    | [] -> Ok (List.rev acc)
    | head :: rest ->
        match head with
        | Error err -> Error err
        | Ok value  -> unfold_res (value :: acc) rest

let unfold_res = unfold_res []

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
    
    | Call(name, args) ->
        (* TODO : more detailed error handling *)
        let rec find_by_first name = function
        | [] -> None
        | (x, func_type, args_types) :: _ when x = name -> Some (func_type, args_types)
        | _ :: rest -> find_by_first name rest in
            
        match find_by_first name state.funcs with
        | None -> Error Was_Not_defined
        | Some (func_type, expected_args_types) ->
            let actual_args_types_res = List.map (fun arg -> infer_expression state arg) args in
            match unfold_res actual_args_types_res with
            | Error err -> Error err
            | Ok (actual_args_types) -> 
                if actual_args_types = expected_args_types (* TODO: somehow process TNull *)
                then Ok (func_type)
                else Error (Function_type_dismatch)

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

let str_of_expr_type = function
    | TInt  -> "Integer"
    | TStr  -> "String"
    | TBool -> "Boolean"
    | TNull -> "Not specified"

let str_of_error = function
    | Already_declared       -> "Already_declared"
    | Was_Not_declared       -> "Was_Not_declared"
    | Was_Not_defined        -> "Was_Not_defined"
    | Was_Not_assigned       -> "Was_Not_assigned"
    | Operand_type_dismatch  -> "Operand_type_dismatch"
    | Function_type_dismatch -> "Function_type_dismatch"
    | Already_specified      -> "Already_specified"

let record var_name var_type = sprintf "Name: %s | Type: %s" var_name (str_of_expr_type var_type)

let rec print_vars acc = function
    | [] -> printf "%s" (String.concat "\n" acc)
    | (Id var_name, var_type) :: rest ->
        print_vars (record var_name var_type :: acc) rest

let print_vars vars = print_vars [] vars

let pp_types_of_program program =
    match type_check_program init_state program with
    | Error error -> printf "%s\n" (str_of_error error)
    | Ok(state) -> print_vars state.vars;

;;
