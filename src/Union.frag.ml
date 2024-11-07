(******************************************************************************)
(*                                                                            *)
(*                                    Baby                                    *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* Union. *)

(* -------------------------------------------------------------------------- *)

(* The set variant. *)

#ifndef MAP_VARIANT

(* This is the simple, elegant version of [union] given by BFS.

let rec union (t1 : TREE) (t2 : TREE) : TREE =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODENODE(_, _, _, l2, v2, r2)
      let l1, r1 = split13 v2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      join l v2 r

 *)

(* Our implementation of [union] is in the same style as [inter].
   It inherits two features of OCaml's Set library:
   - the tree that seems smaller is split;
   - if a subtree is a singleton then [union] degenerates to [add].
   Furthermore, compared with OCaml's Set library, it is able to exploit
   physical equality when present, and it offers a stronger guarantee
   regarding the preservation of physical equality. *)

(* The recursive function [union] ensures that if the result is
   equal to [t2] then the result is physically equal to [t2]. *)

(* In the case where [t2] is a singleton, we have already checked that
   [t1] is neither empty nor a singleton, so the result of the union
   cannot possibly be equal to [t2]. Thus, the obligation to preserve
   sharing disappears in this case: using [add v2 t1] is safe. *)

(* We are able to use [split13] because, as soon as we know that [v2] is
   present on the right-hand side, we do not care whether it is absent or
   present on the left-hand side. In the map variant, however, we must use
   [split] instead. *)

let rec union (t1 : 'v tree) (t2 : 'v tree) : 'v tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODENODE(l1, v1, r1, l2, v2, r2)
      if BOTH_EMPTY(l1, r1) then add v1 t2 else
      if BOTH_EMPTY(l2, r2) then add v2 t1 else
      let l1, r1 = split13 v2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      let v = v2 in
      if l == l2 && r == r2 then t2 else (* preserve sharing *)
      join l v r

(* This toplevel wrapper tests which of the two arguments seems larger. (With
   weight-balanced trees, this is an exact test. With height-balanced trees,
   it is a heuristic test.) This argument, one may hope, might also be the
   result. Therefore, the recursive function [union] (above) is invoked with
   this argument as its second argument. Compared with [inter], this is the
   other way around. *)

(* The fast path, based on the test [t1 == t2], is sound because [union s s]
   is [s]. *)

let union t1 t2 =
  if t1 == t2 then t1 else (* fast path *)
  if seems_smaller t1 t2 then
    union t1 t2
  else
    union t2 t1

(* In the set variant, [override] is [union]. It is commutative. *)

let override = union

#else

(* -------------------------------------------------------------------------- *)

(* The map variant. *)

(* In accordance with the API of OCaml's Map library, [union] expects a
   function [f], which specifies how two values must be combined.

   Furthermore, the internal functions [union1] and [union] expect a
   Boolean flag [flip], which indicates whether the arguments of [f]
   should be exchanged. *)

(* [union1] can be viewed as a generalization of [add] and [replace]
   where the manner in which the existing element and the new element
   must be combined is determined by the parameters [flip] and [f].
   It can also be viewed as a special case of [union] where the first
   map is a singleton. *)

(* [union1] assumes that [t1] is [singleton (k1, d1)]. Carrying this
   extra parameter lets us save one allocation in the base case
   where the key [k1] does not appear in the tree [t2]. *)

let rec union1
  (flip : bool) (f : key -> 'a -> 'a -> 'a option)
  (k1 : key) (d1 : 'a) (t1 : 'a binding tree)
  (t2 : 'a binding tree) : 'a binding tree =
  match VIEW(t2) with
  | LEAF ->
      (* This is equivalent to [singleton (k1, d1)]. *)
      t1
  | NODE(l2, v2, r2)
      let (k2, d2) = v2 in
      let c = E.compare k1 k2 in
      if c = 0 then
        let k = k2 in
        match COMBINE(flip, f, k, d1, d2) with
        | None ->
            join2_siblings l2 r2
        | Some d ->
            if d == d2 then t2 else
            let v2 = (k, d) in
            join_siblings l2 v2 r2
      else if c < 0 then
        let l'2 = union1 flip f k1 d1 t1 l2 in
        if l2 == l'2 then t2 else join_quasi_siblings l'2 v2 r2
      else
        let r'2 = union1 flip f k1 d1 t1 r2 in
        if r2 == r'2 then t2 else join_quasi_siblings l2 v2 r'2

(* Re-package [union1] to take [(k1, d1)] as a pair. *)

let[@inline] union1 flip f (k1, d1) t1 t2 =
  union1 flip f k1 d1 t1 t2

(* [union] does not offer a physical equality guarantee about its result.
   (This does not seem to be possible. One would need to require [f] to
   preserve physical equality, but it is impossible to predict which
   argument of [f] should be favored.) *)

(* As in the set variant, we try to preserve physical equality with the
   second argument, which is likely to be the larger tree. *)

let rec union
  (flip : bool) (f : key -> 'a -> 'a -> 'a option)
  (t1 : 'a binding tree) (t2 : 'a binding tree) : 'a binding tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | NODENODE(l1, v1, r1, l2, v2, r2)
      if BOTH_EMPTY(l1, r1) then union1 flip f v1 t1 t2 else
      if BOTH_EMPTY(l2, r2) then union1 (not flip) f v2 t2 t1 else
      let (k2, d2) = v2 in
      let l1, od1, r1 = split k2 t1 in
      let l = union flip f l1 l2
      and r = union flip f r1 r2 in
      match od1 with
      | None ->
          let v = v2 in
          if l == l2 && r == r2 then t2 else
          join l v r
      | Some d1 ->
          let k = k2 in
          match COMBINE(flip, f, k, d1, d2) with
          | None ->
              join2 l r
          | Some d ->
              if l == l2 && d == d2 && r == r2 then t2 else
              let v = (k, d) in
              join l v r

(* The toplevel wrapper tests which of the two arguments seems larger. This
   guarantees that we split the smaller tree. (As in the set variant, this
   test is performed just once at the top level, not at every level.) *)

(* The fast path, based on the test [t1 == t2], disappears in the map
   variant. It would be sound if we could require [f x x = Some x]
   for all [x]. *)

let union f t1 t2 =
  if seems_smaller t1 t2 then
    let flip = false in
    union flip f t1 t2
  else
    let flip = true in
    union flip f t2 t1

(* In the [merge] functions, we assume that [f None None] is [None], and
   never actually evaluate [f None None]. *)

(* [merge_no_left] is the special case of [merge] where the left-hand tree
   [t1] is empty. *)

let rec merge_no_left f t2 =
  match VIEW(t2) with
  | LEAF ->
      leaf
  | NODE(l2, v2, r2)
      let (k2, d2) = v2 in
      let k = k2
      and od1 = None
      and od2 = Some d2 in
      ojoin (merge_no_left f l2) k (f k od1 od2) (merge_no_left f r2)

(* [merge_no_right] is the special case of [merge] where the right-hand
   tree [t2] is empty. *)

let rec merge_no_right f t1 =
  match VIEW(t1) with
  | LEAF ->
      leaf
  | NODE(l1, v1, r1)
      let (k1, d1) = v1 in
      let k = k1
      and od1 = Some d1
      and od2 = None in
      ojoin (merge_no_right f l1) k (f k od1 od2) (merge_no_right f r1)

(* [merge] does not attempt to preserve physical equality. *)

(* As soon as we discover that one of the two arguments is an empty tree,
   we switch to [merge_no_left] or [merge_no_right]. This should be more
   efficient, and makes the code simpler. *)

let rec merge f t1 t2 =
  match VIEW(t1), VIEW(t2) with
  | LEAF, LEAF ->
      leaf
  | LEAF, _ ->
      merge_no_left f t2
  | _, LEAF ->
      merge_no_right f t1
  | NODENODE(l1, v1, r1, l2, v2, r2)
      if seems_smaller t1 t2 then
        (* Split the smaller tree, namely [t1]. *)
        let (k2, d2) = v2 in
        let k = k2 in
        let l1, od1, r1 = split k2 t1 in
        let od2 = Some d2 in
        ojoin (merge f l1 l2) k (f k od1 od2) (merge f r1 r2)
      else
        (* Split the smaller tree, namely [t2]. *)
        let (k1, d1) = v1 in
        let k = k1 in
        let od1 = Some d1 in
        let l2, od2, r2 = split k1 t2 in
        ojoin (merge f l1 l2) k (f k od1 od2) (merge f r1 r2)

(* In the map variant, [override] is [union] with priority to the right-hand
   argument. It is not commutative. It is used internally; for example, the
   functions [of_array] and [add_seq] rely on it. *)

let override t1 t2 =
  union (fun _k _v1 v2 -> Some v2) t1 t2

#endif
