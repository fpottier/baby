(* [remove_min_elt_1 l v r] removes the minimum element of the tree
   [NODE(l, v, r)]. *)

let rec remove_min_elt_1 (l : tree) (v : key) (r : tree) : tree =
  match VIEW(l) with
  | LEAF ->
      r
  | NODE(ll, lv, lr) ->
      let l = remove_min_elt_1 ll lv lr in
      join_neighbors l v r

(* [remove_min_elt t] removes the minimum element of the tree [t]. *)

let remove_min_elt (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r) ->
      remove_min_elt_1 l v r

(* [remove_max_elt_1 l v r] removes the maximum element of the tree
   [NODE(l, v, r)]. *)

let rec remove_max_elt_1 (l : tree) (v : key) (r : tree) : tree =
  match VIEW(r) with
  | LEAF ->
      l
  | NODE(rl, rv, rr) ->
      let r = remove_max_elt_1 rl rv rr in
      join_neighbors l v r

(* [remove_max_elt t] removes the maximum element of the tree [t]. *)

let remove_max_elt (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r) ->
      remove_max_elt_1 l v r

(* This is removal in the style of BFS. *)

let remove (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join2 l r
