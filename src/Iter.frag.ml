let rec iter f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      ()
  | NODE(l, v, r) ->
      iter f l; f v; iter f r

let rec fold f (t : tree) accu =
  match VIEW(t) with
  | LEAF ->
      accu
  | NODE(l, v, r) ->
      fold f r (f v (fold f l accu))

let rec for_all p (t : tree) =
  match VIEW(t) with
  | LEAF ->
      true
  | NODE(l, v, r) ->
      p v && for_all p l && for_all p r

let rec exists p (t : tree) =
  match VIEW(t) with
  | LEAF ->
      false
  | NODE(l, v, r) ->
      p v || exists p l || exists p r
