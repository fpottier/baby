(* A linear-time [cardinal] function. *)

let constant_time_cardinal =
  false

let rec cardinal accu (t : tree) : int =
  match VIEW(t) with
  | LEAF ->
      accu
  | NODE(l, _, r) ->
      let accu = accu + 1 in
      let accu = cardinal accu l in
      cardinal accu r

let cardinal (t : tree) : int =
  cardinal 0 t
