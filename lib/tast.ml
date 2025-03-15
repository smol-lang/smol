type tbinding =
  { recurse : bool
  ; ident : Id.t
  ; typ : Type.t
  ; body : texpr
  }

and texpr =
  | UnitLit of Type.t
  | BoolLit of bool * Type.t
  | IntLit of int * Type.t
  | Not of texpr * Type.t
  | Neg of texpr * Type.t
  | And of texpr * texpr * Type.t
  | Or of texpr * texpr * Type.t
  | Add of texpr * texpr * Type.t
  | Sub of texpr * texpr * Type.t
  | Mul of texpr * texpr * Type.t
  | Div of texpr * texpr * Type.t
  | Mod of texpr * texpr * Type.t
  | Eq of texpr * texpr * Type.t
  | Leq of texpr * texpr * Type.t
  | Lets of
      { bindings : tbinding list
      ; nest_in : texpr
      ; typ : Type.t
      }
  | Var of Id.t * Type.t
  | Lambda of
      { ident : Id.t
      ; param_type : Type.t
      ; body : texpr
      ; typ : Type.t
      }
  | App of texpr * texpr * Type.t
  | If of
      { cond : texpr
      ; branch_true : texpr
      ; branch_false : texpr
      ; typ : Type.t
      }
[@@deriving show { with_path = false }]

let extract_type = function
  | UnitLit t -> t
  | BoolLit (_, t) -> t
  | IntLit (_, t) -> t
  | Not (_, t) -> t
  | Neg (_, t) -> t
  | And (_, _, t) -> t
  | Or (_, _, t) -> t
  | Add (_, _, t) -> t
  | Sub (_, _, t) -> t
  | Mul (_, _, t) -> t
  | Div (_, _, t) -> t
  | Mod (_, _, t) -> t
  | Eq (_, _, t) -> t
  | Leq (_, _, t) -> t
  | Lets { typ; _ } -> typ
  | Var (_, t) -> t
  | Lambda { typ; _ } -> typ
  | App (_, _, t) -> t
  | If { typ; _ } -> typ
;;
