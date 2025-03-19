open Ast
module IdMap = Map.Make(String)

let rec alpha_convert env = function
  | Unit | Bool _ | Int _ as e -> e
  | Not e -> Not (alpha_convert env e)
  | Neg e -> Neg (alpha_convert env e)
  | And (e1, e2) -> And (alpha_convert env e1, alpha_convert env e2)
  | Or (e1, e2) -> Or (alpha_convert env e1, alpha_convert env e2)
  | Add (e1, e2) -> Add (alpha_convert env e1, alpha_convert env e2)
  | Sub (e1, e2) -> Sub (alpha_convert env e1, alpha_convert env e2)
  | Mul (e1, e2) -> Mul (alpha_convert env e1, alpha_convert env e2)
  | Div (e1, e2) -> Div (alpha_convert env e1, alpha_convert env e2)
  | Mod (e1, e2) -> Mod (alpha_convert env e1, alpha_convert env e2)
  | Leq (e1, e2) -> Leq (alpha_convert env e1, alpha_convert env e2)
  | Eq (e1, e2) -> Eq (alpha_convert env e1, alpha_convert env e2)
  | Lets { bindings; nest_in } ->
      let new_env = List.fold_left 
        (fun env { ident; _ } -> IdMap.add ident (Id.gen_id ident) env) 
        env bindings 
      in
      let new_bindings = List.map 
        (fun { recurse; ident; body } -> 
          { recurse; ident = IdMap.find ident new_env; body = alpha_convert new_env body }) 
        bindings 
      in
      Lets { bindings = new_bindings; nest_in = alpha_convert new_env nest_in }
  | Var id -> Var (match IdMap.find_opt id env with Some new_id -> new_id | None -> id)
  | Lambda { ident; body } ->
      let new_id = Id.gen_id ident in
      let new_env = IdMap.add ident new_id env in
      Lambda { ident = new_id; body = alpha_convert new_env body }
  | App (e1, e2) -> App (alpha_convert env e1, alpha_convert env e2)
  | If { cond; branch_true; branch_false } ->
    let cond' = alpha_convert env cond in
    let branch_true' = alpha_convert env branch_true in
    let branch_false' = alpha_convert env branch_false in
    If { cond = cond'; branch_true = branch_true'; branch_false = branch_false' }



let alpha_convert_top e =
  let result = alpha_convert IdMap.empty e in
  Printf.printf "Alpha-converted: %s\n" (show_expr result);
  result
