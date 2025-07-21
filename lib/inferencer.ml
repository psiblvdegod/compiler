open Types

type err =
    | Already_declared
    | Already_defined
    | Was_not_declared
    | Was_not_defined
    | Type_discrepancy
    | Var_has_no_value
    | Unused_value

type expr_type =
    | TInt
    | TBool
    | TStr
    | TNull

(* TODO: local scopes *)
type state = {
    vars : (string * expr_type) list;
    funcs : (string * (expr_type list)) list;
}

let reg_var state var var_type = { vars = (var, var_type) :: state.vars; funcs = state.funcs}

let reg_func state name args_types = { vars = state.vars; funcs = (name, args_types) :: state.funcs}

let init_state = { vars = []; funcs = []; }

let inf_var state var expected_type = 
    match List.assoc_opt var state.vars with
    | None -> Error Was_not_declared
    | Some t ->
        match t with
        | x when x = expected_type -> Ok expected_type
        | TNull -> Error Var_has_no_value
        | _     -> Error Type_discrepancy

let rec inf_int_expr state = function
    | Int _ -> Ok TInt
    | Var var -> inf_var state var TInt
    | BinOp (_, left, right) ->
        (
        match inf_int_expr state left with
        | Error er -> Error er
        | Ok _ -> inf_int_expr state right
        )
    | Neg expr -> inf_int_expr state expr

let rec inf_str_expr state = function
    | Str _ -> Ok TStr
    | Var var -> inf_var state var TStr
    | StrOp (op) ->
        (
        match op with
        | Rev expr -> inf_str_expr state expr
        | _ -> failwith "not implemented"
        )

let rec inf_bool_expr state = function
    | True | False -> Ok TBool
    | Var var -> inf_var state var TBool
    | IntCmp (_, left, right) ->
        (
        match inf_int_expr state left with
        | Error er -> Error er
        | _ -> inf_int_expr state right
        )
    | StrCmp (left, right) ->
        (
        match inf_str_expr state left with
        | Error er -> Error er
        | _ -> inf_str_expr state right
        )

let inf_expr state = function
    | Int_Expr expr  -> inf_int_expr state expr
    | Str_Expr expr  -> inf_str_expr state expr
    | Bool_Expr expr -> inf_bool_expr state expr

let rec inf_program state = function
    | [] -> Ok (state)
    | statement :: rest ->
        (
            let check = function
            | Declaration vars -> inf_declaration state vars
            | Assignment (var, expr) -> inf_assignment state var expr
            | While (condition, program) -> inf_while state condition program
            | Call (name, args) -> inf_call state name args 
            | Ite (condition, then_program, else_program) -> inf_ite state condition then_program else_program
            | Definition (name, args, program) -> inf_definition state name args program

            in match check statement with
            | Error err -> Error err
            | Ok state -> inf_program state rest
        )

and inf_assignment state var expr =
    match inf_expr state expr with
    | Error err -> Error err
    | Ok var_type -> Ok(reg_var state var var_type)

and inf_while state condition program =
    match inf_bool_expr state condition with
    | Error err -> Error err
    | Ok _ -> inf_program state program

and inf_ite state condition then_program else_program =
    match inf_bool_expr state condition with
    | Error err -> Error err
    | _ ->
        match inf_program state then_program with
        | Error err -> Error err
        | _ -> inf_program state else_program

and inf_declaration state names =
    let rec loop state = function
    | [] -> Ok (state)
    | var :: rest ->
        if List.mem_assoc var state.vars
        then Error Already_declared
        else loop (reg_var state var TNull) rest in
    
    loop state names

(* TODO: process shadowing somehow *)
(* TODO: nested functions ? *)
(* TODO: closure by using state as init_state ?? *)
and inf_definition state name args program =
    
    if List.mem_assoc name state.funcs
    then Error Already_defined
    else

    match inf_declaration init_state args with
    | Error err -> Error err
    | Ok local_state -> 
    match inf_program local_state program with
    | Error err -> Error err
    | Ok local_state -> 
        let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | arg :: rest ->
            match List.assoc arg local_state.vars with
            | TNull -> Error Unused_value
            | arg_type -> loop (arg_type :: acc) rest in
        match loop [] args with
        | Error err -> Error err
        | Ok args_types -> Ok (reg_func state name args_types)

and inf_call state func args =
    match List.assoc_opt func state.funcs with
    | None -> Error Was_not_defined
    | Some expected_args_types ->
        let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | arg :: rest ->
            match inf_expr state arg with
            | Error err -> Error err
            | Ok arg_type -> loop (arg_type :: acc) rest in                
        
        match loop [] args with
        | Error err -> Error err
        | Ok actual_args_types -> 
        if actual_args_types = expected_args_types then Ok state else Error Type_discrepancy
