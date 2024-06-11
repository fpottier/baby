let rec find (x : key) (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        v
      else if c < 0 then
        find x l
      else
        find x r

let rec find_opt (x : key) (t : tree) : key option =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        Some v
      else if c < 0 then
        find_opt x l
      else
        find_opt x r
