open Tast

exception Runtime_error of string

let lookup env id =
  match List.assoc_opt id env with
  | Some v -> v
  | None -> raise (Runtime_error ("Unbound variable: " ^ id))
;;

let rec eval env expr =
  match expr with
  | UnitLit _ -> Value.UnitVal
  | BoolLit (b, _) -> Value.BoolVal b
  | IntLit (i, _) -> Value.IntVal i
  | Not (e, _) ->
    (match eval env e with
     | Value.BoolVal b -> Value.BoolVal (not b)
     | _ -> raise (Runtime_error "Not operator expects a boolean"))
  | Neg (e, _) ->
    (match eval env e with
     | Value.IntVal i -> Value.IntVal (-i)
     | _ -> raise (Runtime_error "Negation operator expects an integer"))
  | And (e1, e2, _) ->
    (match eval env e1 with
     | Value.BoolVal false -> Value.BoolVal false
     | Value.BoolVal true ->
       (match eval env e2 with
        | Value.BoolVal b -> Value.BoolVal b
        | _ -> raise (Runtime_error "And operator expects booleans"))
     | _ -> raise (Runtime_error "And operator expects booleans"))
  | Or (e1, e2, _) ->
    (match eval env e1 with
     | Value.BoolVal true -> Value.BoolVal true
     | Value.BoolVal false ->
       (match eval env e2 with
        | Value.BoolVal b -> Value.BoolVal b
        | _ -> raise (Runtime_error "Or operator expects booleans"))
     | _ -> raise (Runtime_error "Or operator expects booleans"))
  | Add (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.IntVal i1, Value.IntVal i2 -> Value.IntVal (i1 + i2)
     | _ -> raise (Runtime_error "Addition expects integers"))
  | Sub (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.IntVal i1, Value.IntVal i2 -> Value.IntVal (i1 - i2)
     | _ -> raise (Runtime_error "Subtraction expects integers"))
  | Mul (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.IntVal i1, Value.IntVal i2 -> Value.IntVal (i1 * i2)
     | _ -> raise (Runtime_error "Multiplication expects integers"))
  | Div (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.IntVal _, Value.IntVal 0 -> raise (Runtime_error "Division by zero")
     | Value.IntVal i1, Value.IntVal i2 -> Value.IntVal (i1 / i2)
     | _ -> raise (Runtime_error "Division expects integers"))
  | Mod (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.IntVal _, Value.IntVal 0 -> raise (Runtime_error "Modulo by zero")
     | Value.IntVal i1, Value.IntVal i2 -> Value.IntVal (i1 mod i2)
     | _ -> raise (Runtime_error "Modulo expects integers"))
  | Eq (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.UnitVal, Value.UnitVal -> Value.BoolVal true
     | Value.BoolVal b1, Value.BoolVal b2 -> Value.BoolVal (b1 = b2)
     | Value.IntVal i1, Value.IntVal i2 -> Value.BoolVal (i1 = i2)
     | _, _ -> Value.BoolVal false)
  | Leq (e1, e2, _) ->
    (match eval env e1, eval env e2 with
     | Value.IntVal i1, Value.IntVal i2 -> Value.BoolVal (i1 <= i2)
     | _ -> raise (Runtime_error "Less than or equal operator expects integers"))
  | Lets { bindings; nest_in; _ } ->
    (* Handle multiple let bindings *)
    let new_env = eval_bindings env bindings in
    eval new_env nest_in
  | Var (id, _) -> lookup env id
  | Lambda { ident; body; _ } -> Value.ClosureVal { param = ident; body; env }
  | App (e1, e2, _) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match v1 with
     | Value.ClosureVal { param; body; env = closure_env } ->
       let new_env = (param, v2) :: closure_env in
       eval new_env body
     | Value.RecClosureVal { fun_name; param; body; env = closure_env } ->
       let new_env = (param, v2) :: (fun_name, v1) :: closure_env in
       eval new_env body
     | _ -> raise (Runtime_error "Application of non-function"))
  | If { cond; branch_true; branch_false; _ } ->
    (match eval env cond with
     | Value.BoolVal true -> eval env branch_true
     | Value.BoolVal false -> eval env branch_false
     | _ -> raise (Runtime_error "If condition must be a boolean"))

and eval_bindings env bindings =
  match bindings with
  | [] -> env
  | binding :: rest ->
    let new_env =
      if binding.recurse
      then (
        let rec_closure =
          Value.RecClosureVal
            { fun_name = binding.ident
            ; param =
                (match binding.body with
                 | Lambda { ident; _ } -> ident
                 | _ -> raise (Runtime_error "Recursive binding must be a function"))
            ; body =
                (match binding.body with
                 | Lambda { body; _ } -> body
                 | _ -> raise (Runtime_error "Recursive binding must be a function"))
            ; env
            }
        in
        (binding.ident, rec_closure) :: env)
      else (binding.ident, eval env binding.body) :: env
    in
    eval_bindings new_env rest
;;
