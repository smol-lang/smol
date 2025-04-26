type t =
  | UnitVal
  | BoolVal of bool
  | IntVal of int
  | ClosureVal of
      { param : Id.t
      ; body : Tast.texpr
      ; env : env
      }
  | RecClosureVal of
      { fun_name : Id.t
      ; param : Id.t
      ; body : Tast.texpr
      ; env : env
      }

and env = (Id.t * t) list

let rec string_of_value = function
  | UnitVal -> "()"
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | ClosureVal _ -> "<function>"
  | RecClosureVal _ -> "<rec function>"
;;
