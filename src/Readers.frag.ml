let rec mem (x : key) (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      false
  | NODE(l, v, r) ->
      let c = E.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

let rec elements (t : tree) (k : elt list) : elt list =
  match VIEW(t) with
  | LEAF ->
      k
  | NODE(l, v, r) ->
      elements l (v :: elements r k)

let elements (t : tree) : elt list =
  elements t []
