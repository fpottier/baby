(* The union algorithm proposed by BFS is improved (or complicated)
   with two features of OCaml's Set library:
   - the subtree that seems smaller is split (this is a heuristic);
   - if one subtree is a singleton then [union] degenerates to [add]. *)

let rec union (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      if seems_smaller t2 t1 then
        if is_singleton t2 then add k2 t1 else
        let l2, _, r2 = split k1 t2 in
        let l = union l1 l2
        and r = union r1 r2 in
        node l k1 r
      else
        if is_singleton t1 then add k1 t2 else
        let l1, _, r1 = split k2 t1 in
        let l = union l1 l2
        and r = union r1 r2 in
        node l k2 r

let rec inter (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = inter l1 l2
      and r = inter r1 r2 in
      if b then node l k2 r else join2 l r

let rec diff (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      leaf
  | _, LEAF ->
      t1
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, _, r1 = split k2 t1 in
      let l = diff l1 l2
      and r = diff r1 r2 in
      join2 l r

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
      if b then join2 l r else node l k2 r
