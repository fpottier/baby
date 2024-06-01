(* The union algorithm proposed by BFS is improved (or complicated)
   with two features of OCaml's Set library:
   - the subtree that seems smaller is split (this is a heuristic);
   - if one subtree is a singleton then [union] degenerates to [add].

   Whereas OCaml tests which subtree seems smaller at every step in
   the recursive function, we do so only once at the beginning. This
   avoids code duplication and seems to work just as well. (Because
   the trees are balanced, in the recursive function, the property
   that [t1] is (roughly) the smaller tree is preserved.) *)

let rec union (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODE(_, k1, _), NODE(l2, k2, r2) ->
      if is_singleton t1 then add k1 t2 else
      let l1, r1 = split2 k2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      join l k2 r

let union (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODE(_, _, _), NODE(_, _, _) ->
      if seems_smaller t2 t1 then
        union t2 t1
      else
        union t1 t2

let rec inter (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = inter l1 l2
      and r = inter r1 r2 in
      if b then join l k2 r else join2 l r

(* TODO optimize? *)
let rec disjoint (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      not b && disjoint l1 l2 && disjoint r1 r2

let rec diff (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      leaf
  | _, LEAF ->
      t1
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, r1 = split2 k2 t1 in
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
      if b then join2 l r else join l k2 r
