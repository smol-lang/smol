module NameMap = Map.Make (String)

let free_vars typ =
  let rec fvs acc = function
    | Type.Unit | Type.Bool | Type.Int -> acc
    | Type.Fun (t1, t2) -> fvs (fvs acc t1) t2
    | Type.Poly n -> n :: acc
  in
  List.sort_uniq compare (fvs [] typ)
;;

let free_vars_env env =
  NameMap.fold
    (fun _ (Type.Scheme (bound, typ)) acc ->
       let typ_fvs = free_vars typ in
       let unbound = List.filter (fun v -> not (List.mem v bound)) typ_fvs in
       unbound @ acc)
    env
    []
;;

let rec apply_subst subst = function
  | (Type.Unit | Type.Bool | Type.Int) as typ -> typ
  | Type.Fun (t1, t2) -> Type.Fun (apply_subst subst t1, apply_subst subst t2)
  | Type.Poly n ->
    (match List.assoc_opt n subst with
     | Some t -> apply_subst subst t
     | None -> Type.Poly n)
;;

let generalize env typ =
  let env_fvs = free_vars_env env in
  let typ_fvs = free_vars typ in
  let quantified = List.filter (fun v -> not (List.mem v env_fvs)) typ_fvs in
  Type.Scheme (quantified, typ)
;;

let instantiate (Type.Scheme (vars, typ)) =
  let fresh_vars = List.map (fun _ -> Type.gen_type ()) vars in
  let subst = List.combine vars fresh_vars in
  apply_subst subst typ
;;

let rec annotate env = function
  | Ast.Unit -> Tast.UnitLit Type.Unit
  | Ast.Bool b -> Tast.BoolLit (b, Type.Bool)
  | Ast.Int i -> Tast.IntLit (i, Type.Int)
  | Ast.Not e -> Tast.Not (annotate env e, Type.Bool)
  | Ast.Neg e -> Tast.Neg (annotate env e, Type.Int)
  | Ast.And (e1, e2) -> Tast.And (annotate env e1, annotate env e2, Type.Bool)
  | Ast.Or (e1, e2) -> Tast.Or (annotate env e1, annotate env e2, Type.Bool)
  | Ast.Add (e1, e2) -> Tast.Add (annotate env e1, annotate env e2, Type.Int)
  | Ast.Sub (e1, e2) -> Tast.Sub (annotate env e1, annotate env e2, Type.Int)
  | Ast.Mul (e1, e2) -> Tast.Mul (annotate env e1, annotate env e2, Type.Int)
  | Ast.Div (e1, e2) -> Tast.Div (annotate env e1, annotate env e2, Type.Int)
  | Ast.Mod (e1, e2) -> Tast.Mod (annotate env e1, annotate env e2, Type.Int)
  | Ast.Eq (e1, e2) -> Tast.Eq (annotate env e1, annotate env e2, Type.Bool)
  | Ast.Leq (e1, e2) -> Tast.Leq (annotate env e1, annotate env e2, Type.Bool)
  | Ast.Lets { bindings; nest_in } ->
    (* Step 1: Extend env with fresh type variables for all bindings *)
    let env' =
      List.fold_left
        (fun env' Ast.{ ident; body = _; recurse = _ } ->
           NameMap.add ident (Type.Scheme ([], Type.gen_type ())) env')
        env
        bindings
    in
    (* Step 2: Annotate bodies *)
    let tbindings =
      List.map
        (fun Ast.{ ident; body; recurse } ->
           let tbody = annotate env' body in
           let typ = Tast.extract_type tbody in
           (* Generalize the type after annotation *)
           let _ = generalize env' typ in
           Tast.{ recurse; ident; body = tbody; typ })
        bindings
    in
    (* Step 3: Update env with generalized schemes *)
    let env'' =
      List.fold_left
        (fun env'' Tast.{ ident; typ; _ } ->
           NameMap.add ident (generalize env' typ) env'')
        env
        tbindings
    in
    let tnest_in = annotate env'' nest_in in
    Tast.Lets
      { bindings = tbindings; nest_in = tnest_in; typ = Tast.extract_type tnest_in }
  | Ast.Var x ->
    let scheme = NameMap.find x env in
    let typ = instantiate scheme in
    Tast.Var (x, typ)
  | Ast.Lambda { ident; body } ->
    let param_type = Type.gen_type () in
    let env' = NameMap.add ident (Type.Scheme ([], param_type)) env in
    let tbody = annotate env' body in
    let return_type = Tast.extract_type tbody in
    Tast.Lambda
      { ident; param_type; body = tbody; typ = Type.Fun (param_type, return_type) }
  | Ast.App (e1, e2) ->
    let t1 = annotate env e1 in
    let t2 = annotate env e2 in
    Tast.App (t1, t2, Type.gen_type ())
  | Ast.If { cond; branch_true; branch_false } ->
    let tcond = annotate env cond in
    let tbranch_true = annotate env branch_true in
    let tbranch_false = annotate env branch_false in
    Tast.If
      { cond = tcond
      ; branch_true = tbranch_true
      ; branch_false = tbranch_false
      ; typ = Tast.extract_type tbranch_true
      }
;;

let rec collect_constraints = function
  | Tast.UnitLit _ | Tast.BoolLit (_, _) | Tast.IntLit (_, _) | Tast.Var (_, _) -> []
  | Tast.Not (e, _) -> (Tast.extract_type e, Type.Bool) :: collect_constraints e
  | Tast.Neg (e, _) -> (Tast.extract_type e, Type.Int) :: collect_constraints e
  | Tast.And (e1, e2, _) | Tast.Or (e1, e2, _) ->
    ((Tast.extract_type e1, Type.Bool)
     :: (Tast.extract_type e2, Type.Bool)
     :: collect_constraints e1)
    @ collect_constraints e2
  | Tast.Add (e1, e2, _)
  | Tast.Sub (e1, e2, _)
  | Tast.Mul (e1, e2, _)
  | Tast.Div (e1, e2, _)
  | Tast.Mod (e1, e2, _) ->
    ((Tast.extract_type e1, Type.Int)
     :: (Tast.extract_type e2, Type.Int)
     :: collect_constraints e1)
    @ collect_constraints e2
  | Tast.Eq (e1, e2, _) | Tast.Leq (e1, e2, _) ->
    ((Tast.extract_type e1, Tast.extract_type e2) :: collect_constraints e1)
    @ collect_constraints e2
  | Tast.Lets { bindings; nest_in; typ } ->
    let binding_constraints =
      List.map
        (fun Tast.{ recurse = _; ident = _; body; typ } -> typ, Tast.extract_type body)
        bindings
    in
    ((typ, Tast.extract_type nest_in) :: binding_constraints)
    @ List.concat (List.map (fun b -> collect_constraints b.Tast.body) bindings)
    @ collect_constraints nest_in
  | Tast.Lambda { param_type; body; typ; _ } ->
    let return_type = Tast.extract_type body in
    (typ, Type.Fun (param_type, return_type)) :: collect_constraints body
  | Tast.App (e1, e2, result_type) ->
    let func_type = Tast.extract_type e1 in
    let arg_type = Tast.extract_type e2 in
    ((func_type, Type.Fun (arg_type, result_type)) :: collect_constraints e1)
    @ collect_constraints e2
  | Tast.If { cond; branch_true; branch_false; typ } ->
    ((Tast.extract_type cond, Type.Bool)
     :: (Tast.extract_type branch_true, typ)
     :: (Tast.extract_type branch_false, typ)
     :: collect_constraints cond)
    @ collect_constraints branch_true
    @ collect_constraints branch_false
;;

let rec occurs var_id = function
  | Type.Unit | Type.Bool | Type.Int -> false
  | Type.Fun (t1, t2) -> occurs var_id t1 || occurs var_id t2
  | Type.Poly n -> n = var_id
;;

let rec unify t1 t2 =
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int -> []
  | Type.Fun (param1, ret1), Type.Fun (param2, ret2) ->
    let s1 = unify param1 param2 in
    let s2 = unify (apply_subst s1 ret1) (apply_subst s1 ret2) in
    compose_subst s2 s1
  | Type.Poly n, t | t, Type.Poly n -> unify_var n t
  | _ ->
    failwith
      (Printf.sprintf "Type error: Cannot unify %s with %s" (Type.show t1) (Type.show t2))

and unify_var var_id = function
  | Type.Poly m when m = var_id -> []
  | _ as typ ->
    if occurs var_id typ
    then
      failwith
        (Printf.sprintf "Occurs check failed: %d occurs in %s" var_id (Type.show typ))
    else [ var_id, typ ]

and compose_subst s1 s2 =
  let s2' = List.map (fun (n, t) -> n, apply_subst s1 t) s2 in
  let s1_filtered = List.filter (fun (n, _) -> not (List.mem_assoc n s2')) s1 in
  s1_filtered @ s2'
;;

let unify_constraints constraints =
  List.fold_left
    (fun subst (t1, t2) ->
       let new_subst = unify (apply_subst subst t1) (apply_subst subst t2) in
       compose_subst new_subst subst)
    []
    constraints
;;

let rec apply_subst_texpr subst texpr =
  let apply_to_type = apply_subst subst in
  match texpr with
  | Tast.UnitLit t -> Tast.UnitLit (apply_to_type t)
  | Tast.BoolLit (b, t) -> Tast.BoolLit (b, apply_to_type t)
  | Tast.IntLit (i, t) -> Tast.IntLit (i, apply_to_type t)
  | Tast.Not (e, t) -> Tast.Not (apply_subst_texpr subst e, apply_to_type t)
  | Tast.Neg (e, t) -> Tast.Neg (apply_subst_texpr subst e, apply_to_type t)
  | Tast.And (e1, e2, t) ->
    Tast.And (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Or (e1, e2, t) ->
    Tast.Or (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Add (e1, e2, t) ->
    Tast.Add (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Sub (e1, e2, t) ->
    Tast.Sub (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Mul (e1, e2, t) ->
    Tast.Mul (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Div (e1, e2, t) ->
    Tast.Div (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Mod (e1, e2, t) ->
    Tast.Mod (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Eq (e1, e2, t) ->
    Tast.Eq (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Leq (e1, e2, t) ->
    Tast.Leq (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.Lets { bindings; nest_in; typ } ->
    let new_bindings =
      List.map
        (fun Tast.{ recurse; ident; body; typ } ->
           Tast.
             { recurse
             ; ident
             ; body = apply_subst_texpr subst body
             ; typ = apply_to_type typ
             })
        bindings
    in
    Tast.Lets
      { bindings = new_bindings
      ; nest_in = apply_subst_texpr subst nest_in
      ; typ = apply_to_type typ
      }
  | Tast.Var (id, t) -> Tast.Var (id, apply_to_type t)
  | Tast.Lambda { ident; param_type; body; typ } ->
    Tast.Lambda
      { ident
      ; param_type = apply_to_type param_type
      ; body = apply_subst_texpr subst body
      ; typ = apply_to_type typ
      }
  | Tast.App (e1, e2, t) ->
    Tast.App (apply_subst_texpr subst e1, apply_subst_texpr subst e2, apply_to_type t)
  | Tast.If { cond; branch_true; branch_false; typ } ->
    Tast.If
      { cond = apply_subst_texpr subst cond
      ; branch_true = apply_subst_texpr subst branch_true
      ; branch_false = apply_subst_texpr subst branch_false
      ; typ = apply_to_type typ
      }
;;

let init_env =
  NameMap.empty
  |> NameMap.add "print_int" (Type.Scheme ([], Type.Fun (Type.Int, Type.Unit)))
;;

let infer ast =
  let tast = annotate init_env ast in
  let constraints = collect_constraints tast in
  let subst = unify_constraints constraints in
  apply_subst_texpr subst tast
;;
