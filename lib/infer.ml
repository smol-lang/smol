open Type
open Unify

type env = (Id.t * scheme) list

let rec lookup_env env var =
  match env with
  | [] -> raise (TypeError (Printf.sprintf "Unbound variable: %s" var))
  | (var', scheme) :: rest -> if var = var' then scheme else lookup_env rest var
;;

let apply_subst_env subst env =
  List.map (fun (id, scheme) -> id, apply_subst_scheme subst scheme) env
;;

let rec apply_subst_texpr subst texpr =
  match texpr with
  | Tast.UnitLit _ -> Tast.UnitLit Unit
  | Tast.BoolLit (b, _) -> Tast.BoolLit (b, Bool)
  | Tast.IntLit (n, _) -> Tast.IntLit (n, Int)
  | Tast.Not (e, _) ->
    let e' = apply_subst_texpr subst e in
    Tast.Not (e', Bool)
  | Tast.Neg (e, _) ->
    let e' = apply_subst_texpr subst e in
    Tast.Neg (e', Int)
  | Tast.And (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.And (e1', e2', Bool)
  | Tast.Or (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Or (e1', e2', Bool)
  | Tast.Add (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Add (e1', e2', Int)
  | Tast.Sub (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Sub (e1', e2', Int)
  | Tast.Mul (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Mul (e1', e2', Int)
  | Tast.Div (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Div (e1', e2', Int)
  | Tast.Mod (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Mod (e1', e2', Int)
  | Tast.Eq (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Eq (e1', e2', Bool)
  | Tast.Leq (e1, e2, _) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    Tast.Leq (e1', e2', Bool)
  | Tast.Var (id, t) -> Tast.Var (id, apply_subst subst t)
  | Tast.Lambda { ident; param_type; body; typ } ->
    let param_type' = apply_subst subst param_type in
    let body' = apply_subst_texpr subst body in
    let typ' = apply_subst subst typ in
    Tast.Lambda { ident; param_type = param_type'; body = body'; typ = typ' }
  | Tast.App (e1, e2, t) ->
    let e1' = apply_subst_texpr subst e1 in
    let e2' = apply_subst_texpr subst e2 in
    let t' = apply_subst subst t in
    Tast.App (e1', e2', t')
  | Tast.If { cond; branch_true; branch_false; typ } ->
    let cond' = apply_subst_texpr subst cond in
    let branch_true' = apply_subst_texpr subst branch_true in
    let branch_false' = apply_subst_texpr subst branch_false in
    let typ' = apply_subst subst typ in
    Tast.If
      { cond = cond'
      ; branch_true = branch_true'
      ; branch_false = branch_false'
      ; typ = typ'
      }
  | Tast.Lets { bindings; nest_in; typ } ->
    let bindings' = List.map (fun b -> apply_subst_tbinding subst b) bindings in
    let nest_in' = apply_subst_texpr subst nest_in in
    let typ' = apply_subst subst typ in
    Tast.Lets { bindings = bindings'; nest_in = nest_in'; typ = typ' }

and apply_subst_tbinding subst { Tast.recurse; ident; typ; body } =
  { Tast.recurse
  ; ident
  ; typ = apply_subst subst typ
  ; body = apply_subst_texpr subst body
  }
;;

let rec collect_recursive_calls_types ident expr =
  match expr with
  | Tast.UnitLit _ | Tast.BoolLit _ | Tast.IntLit _ -> []
  | Tast.Not (e, _) | Tast.Neg (e, _) -> collect_recursive_calls_types ident e
  | Tast.And (e1, e2, _)
  | Tast.Or (e1, e2, _)
  | Tast.Add (e1, e2, _)
  | Tast.Sub (e1, e2, _)
  | Tast.Mul (e1, e2, _)
  | Tast.Div (e1, e2, _)
  | Tast.Mod (e1, e2, _)
  | Tast.Eq (e1, e2, _)
  | Tast.Leq (e1, e2, _) ->
    collect_recursive_calls_types ident e1 @ collect_recursive_calls_types ident e2
  | Tast.Var (_, _) -> []
  | Tast.Lambda { ident = param_id; body; typ = _; param_type = _ } ->
    if param_id = ident
    then [] (* Shadow the identifier *)
    else collect_recursive_calls_types ident body
  | Tast.App (e1, e2, ret_type) ->
    let types_from_e1 = collect_recursive_calls_types ident e1 in
    let types_from_e2 = collect_recursive_calls_types ident e2 in
    let is_recursive_call =
      match e1 with
      | Tast.Var (id, _) when id = ident -> true
      | _ -> false
    in
    if is_recursive_call
    then (ret_type :: types_from_e1) @ types_from_e2
    else types_from_e1 @ types_from_e2
  | Tast.If { cond; branch_true; branch_false; typ = _ } ->
    collect_recursive_calls_types ident cond
    @ collect_recursive_calls_types ident branch_true
    @ collect_recursive_calls_types ident branch_false
  | Tast.Lets { bindings; nest_in; typ = _ } ->
    let shadowed = List.exists (fun b -> b.Tast.ident = ident) bindings in
    if shadowed
    then [] (* The identifier is shadowed *)
    else (
      let binding_types =
        List.concat_map
          (fun b -> collect_recursive_calls_types ident b.Tast.body)
          bindings
      in
      binding_types @ collect_recursive_calls_types ident nest_in)
;;

let rec find_usage_constraints_in_expr ident expr =
  match expr with
  | Ast.Unit | Ast.Bool _ | Ast.Int _ -> []
  | Ast.Not e -> find_usage_constraints_in_expr ident e
  | Ast.Neg e ->
    let constraints = find_usage_constraints_in_expr ident e in
    if contains_var ident e then (Poly (-1), Int) :: constraints else constraints
  | Ast.And (e1, e2) | Ast.Or (e1, e2) ->
    let c1 = find_usage_constraints_in_expr ident e1 in
    let c2 = find_usage_constraints_in_expr ident e2 in
    let additional =
      if contains_var ident e1 || contains_var ident e2 then [ Poly (-1), Bool ] else []
    in
    c1 @ c2 @ additional
  | Ast.Add (e1, e2)
  | Ast.Sub (e1, e2)
  | Ast.Mul (e1, e2)
  | Ast.Div (e1, e2)
  | Ast.Mod (e1, e2) ->
    let c1 = find_usage_constraints_in_expr ident e1 in
    let c2 = find_usage_constraints_in_expr ident e2 in
    let additional =
      if contains_var ident e1 || contains_var ident e2 then [ Poly (-1), Int ] else []
    in
    c1 @ c2 @ additional
  | Ast.Eq (e1, e2) ->
    let c1 = find_usage_constraints_in_expr ident e1 in
    let c2 = find_usage_constraints_in_expr ident e2 in
    c1 @ c2
  | Ast.Leq (e1, e2) ->
    let c1 = find_usage_constraints_in_expr ident e1 in
    let c2 = find_usage_constraints_in_expr ident e2 in
    let additional =
      if contains_var ident e1 || contains_var ident e2 then [ Poly (-1), Int ] else []
    in
    c1 @ c2 @ additional
  | Ast.Var id -> if id = ident then [] else []
  | Ast.Lambda { ident = lambda_ident; body } ->
    if lambda_ident = ident then [] else find_usage_constraints_in_expr ident body
  | Ast.App (e1, e2) ->
    let c1 = find_usage_constraints_in_expr ident e1 in
    let c2 = find_usage_constraints_in_expr ident e2 in
    c1 @ c2
  | Ast.If { cond; branch_true; branch_false } ->
    let c1 = find_usage_constraints_in_expr ident cond in
    let c2 = find_usage_constraints_in_expr ident branch_true in
    let c3 = find_usage_constraints_in_expr ident branch_false in
    let additional = if contains_var ident cond then [ Poly (-1), Bool ] else [] in
    c1 @ c2 @ c3 @ additional
  | Ast.Lets { bindings; nest_in } ->
    if List.exists (fun b -> b.Ast.ident = ident) bindings
    then []
    else (
      let binding_constraints =
        List.concat_map
          (fun b -> find_usage_constraints_in_expr ident b.Ast.body)
          bindings
      in
      let nest_constraints = find_usage_constraints_in_expr ident nest_in in
      binding_constraints @ nest_constraints)

and contains_var ident expr =
  match expr with
  | Ast.Unit | Ast.Bool _ | Ast.Int _ -> false
  | Ast.Not e | Ast.Neg e -> contains_var ident e
  | Ast.And (e1, e2)
  | Ast.Or (e1, e2)
  | Ast.Add (e1, e2)
  | Ast.Sub (e1, e2)
  | Ast.Mul (e1, e2)
  | Ast.Div (e1, e2)
  | Ast.Mod (e1, e2)
  | Ast.Eq (e1, e2)
  | Ast.Leq (e1, e2) -> contains_var ident e1 || contains_var ident e2
  | Ast.Var id -> id = ident
  | Ast.Lambda { ident = lambda_ident; body } ->
    if lambda_ident = ident then false else contains_var ident body
  | Ast.App (e1, e2) -> contains_var ident e1 || contains_var ident e2
  | Ast.If { cond; branch_true; branch_false } ->
    contains_var ident cond
    || contains_var ident branch_true
    || contains_var ident branch_false
  | Ast.Lets { bindings; nest_in } ->
    if List.exists (fun b -> b.Ast.ident = ident) bindings
    then false
    else
      List.exists (fun b -> contains_var ident b.Ast.body) bindings
      || contains_var ident nest_in
;;

let find_usage_constraints ident remaining_bindings nest_in =
  let binding_constraints =
    List.concat_map
      (fun binding -> find_usage_constraints_in_expr ident binding.Ast.body)
      remaining_bindings
  in
  let nest_constraints = find_usage_constraints_in_expr ident nest_in in
  binding_constraints @ nest_constraints
;;

let rec apply_constraints typ constraints =
  match typ with
  | Poly var ->
    let applicable_constraints =
      List.filter_map
        (fun (poly_type, concrete_type) ->
           match poly_type with
           | Poly _ -> Some concrete_type
           | _ -> None)
        constraints
    in
    (match applicable_constraints with
     | [] -> typ
     | concrete_types ->
       let best_type =
         List.fold_left
           (fun acc t ->
              match acc, t with
              | Poly _, non_poly when not (is_poly non_poly) -> non_poly
              | _, _ -> acc)
           (Poly var)
           concrete_types
       in
       best_type)
  | Fun (t1, t2) ->
    Fun (apply_constraints t1 constraints, apply_constraints t2 constraints)
  | Unit | Bool | Int -> typ

and is_poly = function
  | Poly _ -> true
  | _ -> false
;;

let rec infer env ast =
  match ast with
  | Ast.Unit -> empty_subst, Unit, Tast.UnitLit Unit
  | Ast.Bool b -> empty_subst, Bool, Tast.BoolLit (b, Bool)
  | Ast.Int n -> empty_subst, Int, Tast.IntLit (n, Int)
  | Ast.Not e ->
    let s1, t1, texpr1 = infer env e in
    let s2 = unify t1 Bool in
    let s = compose_subst s2 s1 in
    s, Bool, Tast.Not (apply_subst_texpr s texpr1, Bool)
  | Ast.Neg e ->
    let s1, t1, texpr1 = infer env e in
    let s2 = unify t1 Int in
    let s = compose_subst s2 s1 in
    s, Int, Tast.Neg (apply_subst_texpr s texpr1, Int)
  | Ast.And (e1, e2) ->
    let s1, t1, texpr1 = infer env e1 in
    let s2 = unify t1 Bool in
    let s = compose_subst s2 s1 in
    let env' = apply_subst_env s env in
    let s3, t2, texpr2 = infer env' e2 in
    let s4 = unify t2 Bool in
    let s' = compose_subst s4 (compose_subst s3 s) in
    s', Bool, Tast.And (apply_subst_texpr s' texpr1, apply_subst_texpr s' texpr2, Bool)
  | Ast.Or (e1, e2) ->
    let s1, t1, texpr1 = infer env e1 in
    let s2 = unify t1 Bool in
    let s = compose_subst s2 s1 in
    let env' = apply_subst_env s env in
    let s3, t2, texpr2 = infer env' e2 in
    let s4 = unify t2 Bool in
    let s' = compose_subst s4 (compose_subst s3 s) in
    s', Bool, Tast.Or (apply_subst_texpr s' texpr1, apply_subst_texpr s' texpr2, Bool)
  | Ast.Add (e1, e2)
  | Ast.Sub (e1, e2)
  | Ast.Mul (e1, e2)
  | Ast.Div (e1, e2)
  | Ast.Mod (e1, e2) ->
    let s1, t1, texpr1 = infer env e1 in
    let s2 = unify t1 Int in
    let s = compose_subst s2 s1 in
    let env' = apply_subst_env s env in
    let s3, t2, texpr2 = infer env' e2 in
    let s4 = unify t2 Int in
    let s' = compose_subst s4 (compose_subst s3 s) in
    let texpr1' = apply_subst_texpr s' texpr1 in
    let texpr2' = apply_subst_texpr s' texpr2 in
    let texpr =
      match ast with
      | Ast.Add _ -> Tast.Add (texpr1', texpr2', Int)
      | Ast.Sub _ -> Tast.Sub (texpr1', texpr2', Int)
      | Ast.Mul _ -> Tast.Mul (texpr1', texpr2', Int)
      | Ast.Div _ -> Tast.Div (texpr1', texpr2', Int)
      | Ast.Mod _ -> Tast.Mod (texpr1', texpr2', Int)
      | _ -> failwith "Impossible case"
    in
    s', Int, texpr
  | Ast.Eq (e1, e2) ->
    let s1, t1, texpr1 = infer env e1 in
    let env' = apply_subst_env s1 env in
    let s2, t2, texpr2 = infer env' e2 in
    let s3 = compose_subst s2 s1 in
    let t1' = apply_subst s3 t1 in
    let s4 = unify t1' t2 in
    let s' = compose_subst s4 s3 in
    s', Bool, Tast.Eq (apply_subst_texpr s' texpr1, apply_subst_texpr s' texpr2, Bool)
  | Ast.Leq (e1, e2) ->
    let s1, t1, texpr1 = infer env e1 in
    let s2 = unify t1 Int in
    let s = compose_subst s2 s1 in
    let env' = apply_subst_env s env in
    let s3, t2, texpr2 = infer env' e2 in
    let s4 = unify t2 Int in
    let s' = compose_subst s4 (compose_subst s3 s) in
    s', Bool, Tast.Leq (apply_subst_texpr s' texpr1, apply_subst_texpr s' texpr2, Bool)
  | Ast.Var id ->
    let scheme = lookup_env env id in
    let t = instantiate scheme in
    empty_subst, t, Tast.Var (id, t)
  | Ast.Lambda { ident; body } ->
    let param_type = gen_type () in
    let env' = (ident, scheme_of_type param_type) :: env in
    let s1, body_type, texpr_body = infer env' body in
    let param_type' = apply_subst s1 param_type in
    let fn_type = Fun (param_type', body_type) in
    ( s1
    , fn_type
    , Tast.Lambda
        { ident
        ; param_type = param_type'
        ; body = apply_subst_texpr s1 texpr_body
        ; typ = fn_type
        } )
  | Ast.App (e1, e2) ->
    let s1, t1, texpr1 = infer env e1 in
    let env' = apply_subst_env s1 env in
    let s2, t2, texpr2 = infer env' e2 in
    let s3 = compose_subst s2 s1 in
    let result_type = gen_type () in
    let fn_type = Fun (apply_subst s3 t2, result_type) in
    let s4 = unify (apply_subst s3 t1) fn_type in
    let s' = compose_subst s4 s3 in
    let final_type = apply_subst s' result_type in
    ( s'
    , final_type
    , Tast.App (apply_subst_texpr s' texpr1, apply_subst_texpr s' texpr2, final_type) )
  | Ast.If { cond; branch_true; branch_false } ->
    let s1, t1, texpr_cond = infer env cond in
    let s2 = unify t1 Bool in
    let s = compose_subst s2 s1 in
    let env' = apply_subst_env s env in
    let s3, t2, texpr_true = infer env' branch_true in
    let s' = compose_subst s3 s in
    let env'' = apply_subst_env s' env in
    let s4, t3, texpr_false = infer env'' branch_false in
    let s'' = compose_subst s4 s' in
    let t2' = apply_subst s'' t2 in
    let t3' = apply_subst s'' t3 in
    let s5 = unify t2' t3' in
    let s''' = compose_subst s5 s'' in
    let final_type = apply_subst s''' t2' in
    ( s'''
    , final_type
    , Tast.If
        { cond = apply_subst_texpr s''' texpr_cond
        ; branch_true = apply_subst_texpr s''' texpr_true
        ; branch_false = apply_subst_texpr s''' texpr_false
        ; typ = final_type
        } )
  | Ast.Lets { bindings; nest_in } ->
    let has_mutual_recursion =
      List.length bindings > 1 && List.for_all (fun b -> b.Ast.recurse) bindings
    in
    if has_mutual_recursion
    then (
      let env', subst, typed_bindings = infer_mutual_rec_bindings env bindings in
      let env'' = apply_subst_env subst env' in
      let s', t', texpr' = infer env'' nest_in in
      let final_subst = compose_subst s' subst in
      let final_type = apply_subst final_subst t' in
      let final_texpr = apply_subst_texpr final_subst texpr' in
      let final_bindings =
        List.map
          (fun binding ->
             { binding with
               Tast.typ = apply_subst final_subst binding.Tast.typ
             ; Tast.body = apply_subst_texpr final_subst binding.Tast.body
             })
          typed_bindings
      in
      ( final_subst
      , final_type
      , Tast.Lets { bindings = final_bindings; nest_in = final_texpr; typ = final_type } ))
    else infer_lets env bindings nest_in

and infer_lets env bindings nest_in =
  let rec process_bindings env subst acc_bindings = function
    | [] ->
      let env' = apply_subst_env subst env in
      let s', t', texpr' = infer env' nest_in in
      let final_subst = compose_subst s' subst in
      let final_type = apply_subst final_subst t' in
      let final_texpr = apply_subst_texpr final_subst texpr' in
      let final_bindings =
        List.map
          (fun binding ->
             { binding with
               Tast.typ = apply_subst final_subst binding.Tast.typ
             ; Tast.body = apply_subst_texpr final_subst binding.Tast.body
             })
          (List.rev acc_bindings)
      in
      ( final_subst
      , final_type
      , Tast.Lets { bindings = final_bindings; nest_in = final_texpr; typ = final_type } )
    | { Ast.recurse; ident; body } :: rest ->
      if recurse
      then (
        let temp_type = gen_type () in
        let temp_scheme = scheme_of_type temp_type in
        let temp_env = (ident, temp_scheme) :: env in
        let env' = apply_subst_env subst temp_env in
        let s', t_body, texpr_body = infer env' body in
        let combined_subst = compose_subst s' subst in
        let recursive_call_types = collect_recursive_calls_types ident texpr_body in
        let rec process_constraints curr_subst = function
          | [] -> curr_subst
          | ret_type :: rest ->
            let expected_ret_type =
              match apply_subst curr_subst t_body with
              | Fun (_, ret) -> ret
              | _ -> raise (TypeError "Expected function type for recursive binding")
            in
            let new_subst = unify ret_type expected_ret_type in
            process_constraints (compose_subst new_subst curr_subst) rest
        in
        let constrained_subst = process_constraints combined_subst recursive_call_types in
        let temp_type' = apply_subst constrained_subst temp_type in
        let t_body' = apply_subst constrained_subst t_body in
        let s'' = unify temp_type' t_body' in
        let final_subst = compose_subst s'' constrained_subst in
        let final_type = apply_subst final_subst t_body' in
        let final_type =
          match final_type with
          | Fun (param_type, ret_type) ->
            let concrete_ret_type =
              match ret_type with
              | Poly _ ->
                let usage_constraints = find_usage_constraints ident rest nest_in in
                let ret_constraints =
                  List.filter_map
                    (function
                      | Poly _, concrete
                        when concrete = Int || concrete = Bool || concrete = Unit ->
                        Some concrete
                      | _ -> None)
                    usage_constraints
                in
                (match ret_constraints with
                 | [] -> ret_type
                 | concrete :: _ -> concrete)
              | _ -> ret_type
            in
            Fun (param_type, concrete_ret_type)
          | _ -> final_type
        in
        let gen_env = apply_subst_env final_subst env in
        let scheme = generalize gen_env final_type in
        let new_env = (ident, scheme) :: env in
        let tbinding =
          { Tast.recurse = true
          ; ident
          ; typ = final_type
          ; body = apply_subst_texpr final_subst texpr_body
          }
        in
        process_bindings new_env final_subst (tbinding :: acc_bindings) rest)
      else (
        let env' = apply_subst_env subst env in
        let s', t', texpr_body = infer env' body in
        let combined_subst = compose_subst s' subst in
        let final_type = apply_subst combined_subst t' in
        let final_type =
          match final_type with
          | Poly _ ->
            let usage_constraints = find_usage_constraints ident rest nest_in in
            apply_constraints final_type usage_constraints
          | _ -> final_type
        in
        let gen_env = apply_subst_env combined_subst env in
        let scheme = generalize gen_env final_type in
        let new_env = (ident, scheme) :: env in
        let tbinding =
          { Tast.recurse = false
          ; ident
          ; typ = final_type
          ; body = apply_subst_texpr combined_subst texpr_body
          }
        in
        let updated_env =
          List.map (fun (id, sch) -> id, apply_subst_scheme combined_subst sch) new_env
        in
        process_bindings updated_env combined_subst (tbinding :: acc_bindings) rest)
  in
  process_bindings env empty_subst [] bindings

and infer_mutual_rec_bindings env bindings =
  (* First pass: Create temporary types for all mutually recursive functions *)
  let temp_env_with_types =
    List.fold_left
      (fun (env_acc, types_acc) binding ->
         let temp_type = gen_type () in
         let temp_scheme = scheme_of_type temp_type in
         ( (binding.Ast.ident, temp_scheme) :: env_acc
         , (binding.Ast.ident, temp_type) :: types_acc ))
      (env, [])
      bindings
  in
  let temp_env = fst temp_env_with_types in
  let temp_types = snd temp_env_with_types in
  (* Second pass: Infer types for all functions, with all in scope *)
  let infer_with_constraints env subst =
    List.fold_left
      (fun (env_acc, subst_acc, texpr_acc) binding ->
         let s', t', texpr' = infer env binding.Ast.body in
         let combined_subst = compose_subst s' subst_acc in
         let temp_type =
           try List.assoc binding.Ast.ident temp_types with
           | Not_found -> failwith ("Temporary type not found for " ^ binding.Ast.ident)
         in
         let temp_type' = apply_subst combined_subst temp_type in
         let s'' = unify temp_type' t' in
         let final_subst = compose_subst s'' combined_subst in
         let final_type = apply_subst final_subst t' in
         let tbinding =
           { Tast.recurse = true
           ; ident = binding.Ast.ident
           ; typ = final_type
           ; body = apply_subst_texpr final_subst texpr'
           }
         in
         let gen_env = apply_subst_env final_subst env in
         let scheme = generalize gen_env final_type in
         let new_env = (binding.Ast.ident, scheme) :: env_acc in
         new_env, final_subst, tbinding :: texpr_acc)
      (env, subst, [])
      bindings
  in
  let final_env, final_subst, texpr_bindings =
    infer_with_constraints temp_env empty_subst
  in
  final_env, final_subst, List.rev texpr_bindings
;;

let type_infer expr =
  try
    let expr = Alpha.alpha_convert_top expr in
    let final_subst, typ, texpr = infer [] expr in
    let final_texpr = apply_subst_texpr final_subst texpr in
    apply_subst final_subst typ, final_texpr
  with
  | TypeError msg ->
    prerr_endline ("Type error: " ^ msg);
    exit 1
;;
