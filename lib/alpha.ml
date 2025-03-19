open Ast

type alpha_env = (Id.t * Id.t) list

let rec lookup_var env var =
  match env with
  | [] -> var
  | (old_var, new_var) :: rest -> if old_var = var then new_var else lookup_var rest var
;;

let rec alpha_convert expr = alpha_convert_with_env [] expr

and alpha_convert_with_env env expr =
  match expr with
  | Unit -> Unit
  | Bool b -> Bool b
  | Int n -> Int n
  | Not e -> Not (alpha_convert_with_env env e)
  | Neg e -> Neg (alpha_convert_with_env env e)
  | And (e1, e2) -> And (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Or (e1, e2) -> Or (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Add (e1, e2) -> Add (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Sub (e1, e2) -> Sub (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Mul (e1, e2) -> Mul (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Div (e1, e2) -> Div (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Mod (e1, e2) -> Mod (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Eq (e1, e2) -> Eq (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Leq (e1, e2) -> Leq (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | Var id -> Var (lookup_var env id)
  | Lambda { ident; body } ->
    let fresh_ident = Id.gen_id ident in
    let new_env = (ident, fresh_ident) :: env in
    Lambda { ident = fresh_ident; body = alpha_convert_with_env new_env body }
  | App (e1, e2) -> App (alpha_convert_with_env env e1, alpha_convert_with_env env e2)
  | If { cond; branch_true; branch_false } ->
    If
      { cond = alpha_convert_with_env env cond
      ; branch_true = alpha_convert_with_env env branch_true
      ; branch_false = alpha_convert_with_env env branch_false
      }
  | Lets { bindings; nest_in } ->
    let fresh_bindings =
      List.map
        (fun binding ->
           let fresh_ident = Id.gen_id binding.ident in
           binding.ident, fresh_ident)
        bindings
    in
    let new_env = List.append fresh_bindings env in
    let new_bindings =
      List.map
        (fun binding ->
           let fresh_ident = lookup_var fresh_bindings binding.ident in
           { recurse = binding.recurse
           ; ident = fresh_ident
           ; body = alpha_convert_with_env new_env binding.body
           })
        bindings
    in
    let new_nest_in = alpha_convert_with_env new_env nest_in in
    Lets { bindings = new_bindings; nest_in = new_nest_in }
;;
