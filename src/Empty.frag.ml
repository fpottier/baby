let empty : tree =
  leaf

let is_empty (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      true
  | NODE(_, _, _) ->
      false
