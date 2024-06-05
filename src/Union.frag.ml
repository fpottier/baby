(* -------------------------------------------------------------------------- *)

(* Intersection. *)

(* This is the simple, elegant version of [inter] given by BFS.

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

 *)

(* The recursive function [inter] ensures that if the result is
   equal to [t2] then the result is physically equal to [t2]. *)

(* Compared with the simple version (above),

   + there is a fast path for the case where [t1 == t2] holds;
   + there is specialized code for the case where [t2] is a
     singleton; in that case there is no need to use [split];
   + the code guarantees that if the result is equal to [t2]
     then [t2] itself is returned. *)

let rec inter (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      if t1 == t2 then t2 else (* fast path *)
      if is_singleton t2 then
        (* The tree [t2] is [singleton k2]. *)
        if mem k2 t1 then t2 else leaf
      else
        (* At least one of the subtrees [l2] and [r2] is nonempty. We
           could specialize the following code for the cases where one
           of them is empty, but the performance gain (a few percent)
           is not worth the extra complexity. *)
        let l1, b, r1 = split k2 t1 in
        let l = inter l1 l2
        and r = inter r1 r2 in
        if b then
          if l == l2 && r == r2 then t2 else (* preserve sharing *)
          join l k2 r
        else
          join2 l r

(* This toplevel wrapper serves two purposes. First, it contains a fast path
   for the case where [t1 == t2] holds. Second, it tests which of the two
   arguments seems smaller. (With weight-balanced trees, this is an exact
   test. With height-balanced trees, it is a heuristic test.) This argument,
   one may hope, might also be the result. Therefore, the recursive function
   [inter] (above) is invoked with this argument as its second argument. *)

let inter t1 t2 =
  if t1 == t2 then t1 else (* fast path *)
  if seems_smaller t1 t2 then
    inter t2 t1
  else
    inter t1 t2

(* -------------------------------------------------------------------------- *)

(* Union. *)

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
      let l1, r1 = split13 k2 t1 in
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

(* -------------------------------------------------------------------------- *)

(* Disjointness. *)

(* This simple version of [disjoint] has the same structure as [inter]. *)

(* (Disabled.)

let rec disjoint (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      not b && disjoint l1 l2 && disjoint r1 r2

 *)

(* This more complex version of [disjoint] does not use [split],
   therefore does not construct new trees; it does not allocate
   memory or perform rebalancing work. *)

(* In comparison with [disjoint] in OCaml's Set library, this version
   of [disjoint] can be twice faster and up to 40% slower. *)

(* [disjoint_node t1 l2 v2 r2] tests whether the trees [t1]
   and [NODE(l2, v2, r2)] are disjoint. *)

let rec disjoint_node t1 l2 v2 r2 =
  match VIEW(t1) with
  | LEAF ->
      true
  | NODE(l1, v1, r1) ->
      let c = E.compare v2 v1 in
      if c = 0 then
        false
      else if c < 0 then
        not (mem v1 r2) &&
        disjoint r1 r2 &&
        disjoint_node l1 l2 v2 r2
      else
        not (mem v1 l2) &&
        disjoint l1 l2 &&
        disjoint_node r1 l2 v2 r2

and disjoint (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(_, _, _), NODE(l2, v2, r2) ->
      t1 != t2 && (* fast path *)
      disjoint_node t1 l2 v2 r2

(* -------------------------------------------------------------------------- *)

(* Difference. *)

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

(* -------------------------------------------------------------------------- *)

(* Inclusion. *)

(* This version of [subset] has the same structure as [disjoint] above.
   Compared with [subset] in OCaml's Set library, this version does not
   allocate any memory, and includes a fast path based on a physical
   equality test. It also avoids redundant pattern matching.  *)

let rec subset_node t1 l2 v2 r2 =
  match VIEW(t1) with
  | LEAF ->
      true
  | NODE(l1, v1, r1) ->
      let c = E.compare v2 v1 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        mem v1 r2 &&
        subset r1 r2 &&
        subset_node l1 l2 v2 r2
      else
        mem v1 l2 &&
        subset l1 l2 &&
        subset_node r1 l2 v2 r2

and subset (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODE(_, _, _), NODE(l2, v2, r2) ->
      t1 == t2 || (* fast path *)
      subset_node t1 l2 v2 r2

(* In weight-balanced trees, the weight of a tree can be determined in
   constant time. This yields a fast path: [weight t1 <= weight t2] does not
   hold, then [subset t1 t2] returns false. In height-balanced trees, the
   [weight] function returns a constant value, so this fast path is
   disabled. *)

(* This weight-based fast path is used only at the root. It could be used
   again at every node, but this does not seem to be worthwhile. *)

let subset (t1 : tree) (t2 : tree) : bool =
  weight t1 <= weight t2 && (* fast path *)
  subset t1 t2

(* -------------------------------------------------------------------------- *)

(* Symmetric difference. *)

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
