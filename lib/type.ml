module IntSet = Set.Make (Int)

type t =
  | Unit
  | Bool
  | Int
  | Fun of t * t
  | Poly of int
[@@deriving show { with_path = false }]

type scheme = Scheme of IntSet.t * t

let counter = ref 0

let gen_type () =
  let t = !counter in
  counter := !counter + 1;
  Poly t
;;

let scheme_of_type t = Scheme (IntSet.empty, t)

let rec free_vars = function
  | Unit | Bool | Int -> IntSet.empty
  | Fun (t1, t2) -> IntSet.union (free_vars t1) (free_vars t2)
  | Poly t -> IntSet.singleton t
;;

let rec free_vars_scheme (Scheme (vars, t)) = IntSet.diff (free_vars t) vars
