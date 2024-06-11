(* -------------------------------------------------------------------------- *)

(* Symmetric difference. *)

(* This is a simple, elegant version of [xor].

let rec xor (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = xor l1 l2
      and r = xor r1 r2 in
      if b then join2 l r else join l k2 r

 *)

let rec xor (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      if t1 == t2 then leaf else (* fast path *)
      if BOTH_EMPTY(l2, r2) then
        (* [t2] is [singleton k2]. *)
        if mem k2 t1 then
          remove k2 t1
        else
          add k2 t1
      else
        let l1, b, r1 = split k2 t1 in
        let l = xor l1 l2
        and r = xor r1 r2 in
        if b then
          join2 l r
        else
          if l == l2 && r == r2 then t2 else (* preserve sharing *)
          join l k2 r
