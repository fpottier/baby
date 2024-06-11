(* -------------------------------------------------------------------------- *)

(* Difference. *)

(* This is a simple, elegant version of [diff]. This version splits the
   tree [t1].

let rec diff (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      leaf
  | _, LEAF ->
      t1
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, r1 = split13 k2 t1 in
      let l = diff l1 l2
      and r = diff r1 r2 in
      join2 l r

 *)

(* This version of [diff] guarantees that if the result is equal to [t1]
   then [t1] itself is returned. *)

let rec diff (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      leaf
  | _, LEAF ->
      t1
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      if t1 == t2 then leaf else (* fast path *)
      if BOTH_EMPTY(l1, r1) then
        (* [t1] is [singleton k1]. *)
        if mem k1 t2 then leaf else t1
      else if BOTH_EMPTY(l2, r2) then
        (* [t2] is [singleton k2]. *)
        remove k2 t1
      else
        let l2, b, r2 = split k1 t2 in
        let l = diff l1 l2
        and r = diff r1 r2 in
        if b then
          join2 l r
        else
          if l == l1 && r == r1 then t1 else (* preserve sharing *)
          join l k1 r
