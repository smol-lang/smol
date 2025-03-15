type t = string [@@deriving show]

let counter = ref 0

let gen_id s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter
;;

let rec suffix = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Fun (param, ret) -> Printf.sprintf "%s_%s" (suffix param) (suffix ret)
  | _ -> failwith "not implemented"
;;

let gen_tmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (suffix typ) !counter
;;
