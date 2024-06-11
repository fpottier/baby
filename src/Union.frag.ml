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
      if BOTH_EMPTY(l2, r2) then
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

(* This is the simple, elegant version of [union] given by BFS.

let rec union (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, r1 = split13 k2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      join l k2 r

 *)

(* Our implementation of [union] is in the same style as [inter] (above).
   It inherits two features of OCaml's Set library:
   - the subtree that seems smaller is split;
   - if one subtree is a singleton then [union] degenerates to [add].
   Furthermore, compared with OCaml's Set library, it is able to exploit
   physical equality when present, and it offers a stronger guarantee
   regarding the preservation of physical equality. *)

(* The recursive function [union] ensures that if the result is
   equal to [t2] then the result is physically equal to [t2]. *)

let rec union (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      if BOTH_EMPTY(l1, r1) then add k1 t2 else
      let l1, r1 = split13 k2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      if l == l2 && r == r2 then t2 else (* preserve sharing *)
      join l k2 r

(* This toplevel wrapper tests which of the two arguments seems larger. (With
   weight-balanced trees, this is an exact test. With height-balanced trees,
   it is a heuristic test.) This argument, one may hope, might also be the
   result. Therefore, the recursive function [union] (above) is invoked with
   this argument as its second argument. Compared with [inter], this is the
   other way around. *)

let union t1 t2 =
  if t1 == t2 then t1 else (* fast path *)
  if seems_smaller t1 t2 then
    union t1 t2
  else
    union t2 t1

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

(* The above code can be improved by adding a fast path (based on physical
   equality), by adding special cases for singletons, and by using a copy of
   [split] that does not construct the subtrees [l] and [r] if the Boolean
   result [b] is true. *)

(* I have played with these variations, but I find them to be consistently
   slower than the following approach, which is based on [Enum.disjoint]. *)

let disjoint t1 t2 =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true (* fast path *)
  | _, _ ->
      t1 != t2 && (* fast path *)
      Enum.(disjoint (enum t1) (enum t2))

(* I have also played with a version of [disjoint] that does not use [split],
   therefore does not construct new trees; it does not allocate memory or
   perform rebalancing work. It can be fast, but I believe that its worst-case
   time complexity is not optimal. *)

(* -------------------------------------------------------------------------- *)

(* Inclusion. *)

(* This simple version of [subset] has canonical structure. *)

(* (Disabled.)

let rec subset (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      let l1, r1 = split13 k2 t1 in
      subset l1 l2 && subset r1 r2

 *)

(* This version adds a positive fast path (based on physical equality), a
   negative fast path (based on weights), and a special treatment of the case
   where [t1] is a singleton. (There is no need to add special treatment of
   the case where [t2] is a singleton. Indeed, the subcases where [t1] is
   empty or a singleton are taken care of already, and the subcase where [t1]
   has more than one element is caught by the weight test.) *)

(* In weight-balanced trees, the weight of a tree can be determined in time
   O(1). This yields a negative fast path: if [weight t1 <= weight t2] does
   not hold, then [subset t1 t2] returns false. In height-balanced trees, the
   [weight] function returns a constant value, so this fast path is
   disabled. *)

let rec subset (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      t1 == t2 || (* fast path *)
      if BOTH_EMPTY(l1, r1) then
        (* The tree [t1] is [singleton k1]. *)
        mem k1 t2
      else
        weight t1 <= weight t2 && (* fast path *)
        let l1, r1 = split13 k2 t1 in
        subset l1 l2 && subset r1 r2
