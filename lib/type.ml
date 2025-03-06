type t =
  | Unit
  | Bool
  | Int
  | Var of t option ref
[@@deriving show]

let gen_type () = Var (ref None)
