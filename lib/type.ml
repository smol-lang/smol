type t =
  | Unit
  | Bool
  | Int
  | Fun of t * t
  | Poly of int
[@@deriving show { with_path = false }]

type scheme = Scheme of int list * t [@@deriving show { with_path = false }]

let counter = ref 0

let gen_type () =
  let t = !counter in
  counter := !counter + 1;
  Poly t
;;
