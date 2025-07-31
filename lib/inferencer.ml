(* psiblvdegod, 2025, under MIT License *)

open Types

let rec infer_expression scope = function
  | Int x -> Ok (Type_Int (Typed_value x))
  | Bool x -> Ok (Type_Bool (Typed_value x))
  | Str x -> Ok (Type_Str (Typed_value x))
  | BinOp (binop, left, right) -> (
      match infer_expression scope left with
      | Error err -> Error err
      | Ok typed_left -> (
          match infer_expression scope right with
          | Error err -> Error err
          | Ok typed_right -> infer_binop binop typed_left typed_right))
  | UnOp (unop, operand) -> (
      match infer_expression scope operand with
      | Error err -> Error err
      | Ok typed_operand -> infer_unop unop typed_operand)
  | Var var -> (
      match find_var var scope.vars with
      | Error err -> Error err
      | Ok (_, var_type) -> (
          match var_type with
          | TInt -> Ok (Type_Int (Typed_var var))
          | TBool -> Ok (Type_Bool (Typed_var var))
          | TStr -> Ok (Type_Str (Typed_var var))
          | TNull -> Error Was_Not_assigned))

and infer_binop binop typed_left typed_right =
  match (binop, (typed_left, typed_right)) with
  | (Mul | Div | Add | Sub), (Type_Int _, Type_Int _) ->
      Ok (Type_Int (Typed_binop (binop, typed_left, typed_right)))
  | (Eq | Neq | Leq | Geq | Lt | Gt), (Type_Int _, Type_Int _) ->
      Ok (Type_Bool (Typed_binop (binop, typed_left, typed_right)))
  | (And | Or), (Type_Bool _, Type_Bool _) ->
      Ok (Type_Bool (Typed_binop (binop, typed_left, typed_right)))
  | Cat, (Type_Str _, Type_Str _) ->
      Ok (Type_Str (Typed_binop (binop, typed_left, typed_right)))
  | _ -> Error Operand_type_dismatch

and infer_unop unop typed_operand =
  match (unop, typed_operand) with
  | Neg, Type_Int _ -> Ok (Type_Int (Typed_unop (unop, typed_operand)))
  | Not, Type_Bool _ -> Ok (Type_Bool (Typed_unop (unop, typed_operand)))
  | Rev, Type_Str _ -> Ok (Type_Str (Typed_unop (unop, typed_operand)))
  | _ -> Error Operand_type_dismatch

and find_var name = function
  | [] -> Error Was_Not_declared
  | head :: tail -> (
      match head with k, _ when k = name -> Ok head | _ -> find_var name tail)

let init_scope = { vars = []; funcs = [] }

let rec infer_program scope program acc =
  match program with
  | [] -> Ok (List.rev acc)
  | statement :: rest -> (
      match statement with
      | Declaration names -> (
          match infer_declaration scope names with
          | Error err -> Error err
          | Ok (new_scope, typed_declaration) ->
              infer_program new_scope rest (typed_declaration :: acc))
      | Assignment (name, expr) -> (
          match infer_assignment scope name expr with
          | Error err -> Error err
          | Ok (new_scope, typed_assignment) ->
              infer_program new_scope rest (typed_assignment :: acc))
      | While (condition, program) -> (
          match infer_while scope condition program with
          | Error err -> Error err
          | Ok (new_scope, typed_while) ->
              infer_program new_scope rest (typed_while :: acc))
      | Ite (condition, then_program, else_program) -> (
          match infer_ite scope condition then_program else_program with
          | Error err -> Error err
          | Ok (new_scope, typed_ite) ->
              infer_program new_scope rest (typed_ite :: acc))
      | Call (name, args) -> (
          match infer_call scope name args with
          | Error err -> Error err
          | Ok (new_scope, typed_call) ->
              infer_program new_scope rest (typed_call :: acc))
      | Definition (name, args, program) -> (
          match infer_definition scope name args program with
          | Error err -> Error err
          | Ok (new_scope, definition) ->
              infer_program new_scope rest (definition :: acc)))

and infer_declaration scope vars =
  let has_no_duplicates ls =
    let rec loop = function
      | [] | [ _ ] -> true
      | a :: b :: rest -> if a = b then false else loop (b :: rest)
    in
    loop (List.sort Stdlib.compare ls)
  in
  if has_no_duplicates vars = false then Error Already_declared
  else
    let typed_vars = List.map (fun var -> (var, TNull)) vars in
    let new_scope = { vars = scope.vars @ typed_vars; funcs = scope.funcs } in
    Ok (new_scope, (Typed_Declaration vars, scope))

and infer_assignment scope name expr =
  match infer_expression scope expr with
  | Error err -> Error err
  | Ok typed_expr -> (
      let expr_type =
        match typed_expr with
        | Type_Int _ -> TInt
        | Type_Bool _ -> TBool
        | Type_Str _ -> TStr
      in
      match List.assoc_opt name scope.vars with
      | None -> Error Was_Not_declared
      | Some var_type ->
          if var_type = TNull || var_type = expr_type then
            let vars = replace_assoc scope.vars name expr_type in
            let new_scope = { vars; funcs = scope.funcs } in
            Ok (new_scope, (Typed_Assignment (name, typed_expr), scope))
          else Error Expression_type_dismatch)

and infer_while scope condition program =
  match infer_expression scope condition with
  | Error err -> Error err
  | Ok typed_expr -> (
      match typed_expr with
      | Type_Bool _ -> (
          match infer_program scope program [] with
          | Error err -> Error err
          | Ok typed_program ->
              Ok (scope, (Typed_While (typed_expr, typed_program), scope)))
      | _ -> Error Expression_type_dismatch)

and infer_ite scope condition then_program else_program =
  match infer_expression scope condition with
  | Error err -> Error err
  | Ok typed_expr -> (
      match typed_expr with
      | Type_Bool _ -> (
          match infer_program scope then_program [] with
          | Error err -> Error err
          | Ok typed_then_program -> (
              match infer_program scope else_program [] with
              | Error err -> Error err
              | Ok typed_else_program ->
                  Ok
                    ( scope,
                      ( Typed_Ite
                          (typed_expr, typed_then_program, typed_else_program),
                        scope ) )))
      | _ -> Error Expression_type_dismatch)

(* TODO : check if function was defined *)
and infer_call scope name args =
  let rec loop args acc =
    match args with
    | [] -> Ok (List.rev acc)
    | expr :: rest -> (
        match infer_expression scope expr with
        | Error err -> Error err
        | Ok typed_expr -> loop rest (typed_expr :: acc))
  in
  match loop args [] with
  | Error err -> Error err
  | Ok typed_args -> Ok (scope, (Typed_Call (name, typed_args), scope))

(* TODO : assert that args are unique *)
and infer_definition scope name args program =
  let rec annotate_args args acc =
    match args with
    | [] -> Ok acc
    | (type_name, arg_name) :: rest -> (
        match type_name with
        | "int" -> annotate_args rest ((arg_name, TInt) :: acc)
        | "bool" -> annotate_args rest ((arg_name, TBool) :: acc)
        | "string" -> annotate_args rest ((arg_name, TStr) :: acc)
        | _ -> Error Unknown_type_in_definition)
  in
  match annotate_args args [] with
  | Error err -> Error err
  | Ok typed_args -> (
      let body_scope =
        {
          vars = typed_args @ scope.vars;
          funcs = (name, typed_args) :: scope.funcs;
        }
      in
      match infer_program body_scope program [] with
      | Error err -> Error err
      | Ok typed_program ->
          let new_scope =
            { vars = scope.vars; funcs = (name, typed_args) :: scope.funcs }
          in
          Ok
            ( new_scope,
              (Typed_Definition (name, typed_args, typed_program), body_scope)
            ))

and replace_assoc ls key value =
  let rec loop ls acc =
    match ls with
    | [] -> raise Not_found
    | (k, v) :: tail ->
        if k = key then List.rev acc @ ((key, value) :: tail)
        else loop tail ((k, v) :: acc)
  in
  loop ls []

let infer_types program = infer_program init_scope program []
