type t =
  | Unit
  | Bool
  | Int
  | Fun of t * t
  | Var of t option ref
[@@deriving show]

let gen_type () = Var (ref None)
