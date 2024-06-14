(* [filter] is the same as in OCaml's Set library. *)

(* Because [join] and [join2] have logarithmic cost, this implementation
   of [filter] has linear time complexity. *)

(* One could imagine a completely different implementation of [filter],
   also with linear time complexity, as follows: copy the data to an
   array, filter the array, reconstruct a tree. However, this approach
   would require linear auxiliary storage, may be slower in practice, and
   would be less effective at preserving sharing in scenarios where many
   elements are retained. *)

let rec filter p (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      leaf
  | NODE(l, v, r) ->
      (* Enforce left-to-right evaluation order. *)
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then
        if l == l' && r == r' then t else join l' v r'
      else
        join2 l' r'

(* [partition] is the same as in OCaml's Set library, with one extra
   optimization: as in [filter], we attempt to preserve sharing where
   possible. *)

(* Regarding worst-case time complexity, the above comment about [filter]
   also applies to [partition]. *)

let rec partition p (t : tree) : tree * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, leaf
  | NODE(l, v, r) ->
      (* Enforce left-to-right evaluation order. *)
      let lt, lf = partition p l in
      let pv = p v in
      let rt, rf = partition p r in
      if pv then
        (if lt == l && rt == r then t else join lt v rt),
        join2 lf rf
      else
        join2 lt rt,
        (if lf == l && rf == r then t else join lf v rf)
