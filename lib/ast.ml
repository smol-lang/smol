type binding =
  { recurse : bool
  ; ident : Id.t
  ; body : expr
  }

and expr =
  | Unit
  | Bool of bool
  | Int of int
  | Not of expr
  | Neg of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Lets of
      { bindings : binding list
      ; nest_in : expr
      }
  | Var of Id.t
  | Lambda of
      { ident : Id.t
      ; body : expr
      }
  | App of expr * expr
  | If of
      { cond : expr
      ; branch_true : expr
      ; branch_false : expr
      }
[@@deriving show]
