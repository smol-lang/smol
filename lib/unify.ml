open Type

exception TypeError of string

type subst = (int * t) list

let empty_subst = []

let rec apply_one subst_var subst_type = function
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Fun (t1, t2) ->
    Fun (apply_one subst_var subst_type t1, apply_one subst_var subst_type t2)
  | Poly var -> if var = subst_var then subst_type else Poly var
;;

let rec apply_subst subst typ =
  match subst with
  | [] -> typ
  | (var, t) :: rest -> apply_subst rest (apply_one var t typ)
;;

let compose_subst s1 s2 =
  let s1' = List.map (fun (var, t) -> var, apply_subst s2 t) s1 in
  s1' @ s2
;;

let rec occurs var = function
  | Unit | Bool | Int -> false
  | Fun (t1, t2) -> occurs var t1 || occurs var t2
  | Poly t -> t = var
;;

let rec unify t1 t2 =
  match t1, t2 with
  | Unit, Unit -> empty_subst
  | Bool, Bool -> empty_subst
  | Int, Int -> empty_subst
  | Fun (param1, ret1), Fun (param2, ret2) ->
    let s1 = unify param1 param2 in
    let s2 = unify (apply_subst s1 ret1) (apply_subst s1 ret2) in
    compose_subst s2 s1
  | Poly var, t | t, Poly var ->
    if t = Poly var
    then empty_subst
    else if occurs var t
    then
      raise
        (TypeError
           (Printf.sprintf "Recursive type: type variable %d occurs in %s" var (show t)))
    else [ var, t ]
  | _, _ ->
    raise
      (TypeError
         (Printf.sprintf "Type mismatch: cannot unify %s with %s" (show t1) (show t2)))
;;

let apply_subst_scheme subst (Scheme (vars, t)) =
  let subst' = List.filter (fun (var, _) -> not (IntSet.mem var vars)) subst in
  Scheme (vars, apply_subst subst' t)
;;

let instantiate (Scheme (vars, t)) =
  let var_map =
    IntSet.fold
      (fun var acc ->
         let fresh_var = gen_type () in
         match fresh_var with
         | Poly _ -> (var, fresh_var) :: acc
         | _ -> failwith "Impossible: gen_type returned non-Poly")
      vars
      []
  in
  apply_subst var_map t
;;

let generalize env t =
  let env_free_vars =
    List.fold_left
      (fun acc (_, scheme) -> IntSet.union acc (free_vars_scheme scheme))
      IntSet.empty
      env
  in
  let type_free_vars = free_vars t in
  let vars_to_generalize = IntSet.diff type_free_vars env_free_vars in
  Scheme (vars_to_generalize, t)
;;
