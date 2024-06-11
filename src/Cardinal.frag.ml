(* A linear-time [cardinal] function. *)

let constant_time_cardinal =
  false

let rec cardinal accu (t : tree) : int =
  match t with
  | TLeaf ->
      accu
  | TNode { l; r; _ } ->
      let accu = accu + 1 in
      let accu = cardinal accu l in
      cardinal accu r

let cardinal (t : tree) : int =
  cardinal 0 t
