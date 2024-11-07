(* [join2 l r] requires [l < r]. It joins the trees [l] and [r]
   into a single tree. *)

(* [join2] is known as [concat] in OCaml's Set library. *)

(* This is the code proposed by BFS. Their [split_last] function
   corresponds to our functions [min_elt] and [remove_min_elt_1].
   (Set variant only.)

let rec split_last (l : TREE) (k : key) (r : TREE) : TREE * key =
  match VIEW(r) with
  | LEAF ->
      l, k
  | NODE(l', k', r')
      let r, m = split_last l' k' r' in
      join l k r, m

let join2 (l : TREE) (r : TREE) : TREE =
  match VIEW(l) with
  | LEAF ->
      r
  | NODE(ll, m, lr)
      let l', k = split_last ll m lr in
      join l' k r

 *)

(* [join2 l r] is implemented by extracting the maximum element of [l]
   or the minimum element of [r] and letting [join] do the rest of the
   work. *)

(* In order to maintain a better balance, one might wish to extract an
   element from the tree that seems larger. However, this seems to
   bring no improvement in practice, so we avoid this complication. *)

let join2 (l : TREE) (r : TREE) : TREE =
  match VIEW(l), VIEW(r) with
  | LEAF, _ ->
      r
  | _, LEAF ->
      l
  | _, NODE(rl, rv, rr)
      join
        l
        (min_elt_1 rv rl)           (* same as [min_elt r] *)
        (remove_min_elt_1 rl rv rr) (* same as [remove_min_elt r] *)

(* -------------------------------------------------------------------------- *)

#ifdef MAP_VARIANT

(* [ojoin] is known as [concat_or_join] in OCaml's Map library. *)

(* [ojoin] is defined only in the map variant. *)

let ojoin t1 k od t2 =
  match od with
  | None ->
      join2 t1 t2
  | Some d ->
      join t1 (k, d) t2

#endif
