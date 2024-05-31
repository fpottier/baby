let rec min_elt_1 (default : key) (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      default
  | NODE(l, v, _) ->
      min_elt_1 v l

let min_elt (t : tree) =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, _) ->
      min_elt_1 v l
