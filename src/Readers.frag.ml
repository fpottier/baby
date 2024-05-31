let rec mem (x : key) (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      false
  | NODE(l, v, r) ->
      let c = E.compare x v in
      c = 0 || mem x (if c < 0 then l else r)
