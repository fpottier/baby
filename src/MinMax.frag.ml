let rec min_elt_1 (default : key) (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      default
  | NODE(l, v, _) ->
      min_elt_1 v l

let min_elt (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, _) ->
      min_elt_1 v l

let rec min_elt_opt_1 (default : key) (t : tree) : key option =
  match VIEW(t) with
  | LEAF ->
      Some default
  | NODE(l, v, _) ->
      min_elt_opt_1 v l

let min_elt_opt (t : tree) : key option =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(l, v, _) ->
      min_elt_opt_1 v l

let rec max_elt_1 (default : key) (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      default
  | NODE(_, v, r) ->
      max_elt_1 v r

let max_elt (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(_, v, r) ->
      max_elt_1 v r

let rec max_elt_opt_1 (default : key) (t : tree) : key option =
  match VIEW(t) with
  | LEAF ->
      Some default
  | NODE(_, v, r) ->
      max_elt_opt_1 v r

let max_elt_opt (t : tree) : key option =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(_, v, r) ->
      max_elt_opt_1 v r
