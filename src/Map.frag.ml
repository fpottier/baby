(* [map] is defined in the same way as in OCaml's Set library. *)

let[@inline] tree_below_key (t : tree) (x : key) : bool =
  match VIEW(t) with
  | LEAF ->
      true
  | NODE(_, v, r) ->
      E.compare (max_elt_1 v r) x < 0

let[@inline] key_below_tree (x : key) (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      true
  | NODE(l, v, _) ->
      E.compare x (min_elt_1 v l) < 0

(* [lax_join l v r] is analogous to [join l v r], but does not
   require [l < v < r]. *)

let[@inline] lax_join l v r =
  if tree_below_key l v && key_below_tree v r then
    join l v r
  else
    union l (add v r)

let rec map f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      leaf
  | NODE(l, v, r) ->
     (* We enforce left-to-right evaluation order. *)
     let l' = map f l in
     let v' = f v in
     let r' = map f r in
     if l == l' && v == v' && r == r' then t (* preserve sharing *)
     else lax_join l' v' r'
