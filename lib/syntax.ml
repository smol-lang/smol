type expr =
  | Unit
  | Bool of bool
  | Int of int
  | Not of expr
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Let of (Id.t * Type.t) * expr * expr
  | Var of Id.t
[@@deriving show]
