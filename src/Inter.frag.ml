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

(* Intersection. *)

(* -------------------------------------------------------------------------- *)

(* The set variant. *)

#ifndef MAP_VARIANT

(* This is the simple, elegant version of [inter] given by BFS.

let rec inter (t1 : TREE) (t2 : TREE) : TREE =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODENODE(_, _, _, l2, k2, r2)
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

(* Adding specialized code for the case where [t1] is a singleton can lead
   to small gains or losses in speed; the effect seems unclear. *)

(* Adding specialized code for the cases where one of [l2] or [r2] is empty
   saves a few percent in time, and is not worth the extra complexity. *)

let rec inter (t1 : TREE) (t2 : TREE) : TREE =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODENODE(_, _, _, l2, k2, r2)
      if t1 == t2 then t2 else (* fast path *)
      if BOTH_EMPTY(l2, r2) then
        (* The tree [t2] is [singleton k2]. *)
        if mem k2 t1 then t2 else leaf
      else
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

#else

(* -------------------------------------------------------------------------- *)

(* The map variant. *)

(* Like [union], [inter] expects a function [f], which specifies how two
   values must be combined. In contrast with [union], [inter] admits a more
   general type, where the two input maps and the output map can have three
   distinct types of values. *)

(* The physical equality tests, whose purpose was to preserve sharing, are
   abandoned, because they impose undesirable type equality constraints. *)

(* [inter1l] represents the special case where the left-hand tree is a
   singleton [singleton (k1, d1)]. *)

let inter1l f (k1, d1) t2 =
  match find_opt k1 t2 with
  | None ->
      empty
  | Some d2 ->
      let k = k1 in
      match f k d1 d2 with
      | None ->
          empty
      | Some d ->
          singleton (k, d)

let rec inter f t1 t2 =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODENODE(l1, v1, r1, l2, v2, r2)
      if BOTH_EMPTY(l1, r1) then inter1l f v1 t2 else
      let (k2, d2) = v2 in
      let l1, od1, r1 = split k2 t1 in
      let l = inter f l1 l2
      and r = inter f r1 r2 in
      match od1 with
      | None ->
          join2 l r
      | Some d1 ->
          let k = k2 in
          let od = f k d1 d2 in
          ojoin l k od r

(* The toplevel wrapper tests which of the two arguments seems larger. This
   guarantees that we split the smaller tree. (As in the set variant, this
   test is performed just once at the top level, not at every level.) *)

let inter f t1 t2 =
  if seems_smaller t1 t2 then
    inter f t1 t2
  else
    inter (fun k d1 d2 -> f k d2 d1) t2 t1
      (* We could avoid creating this closure, which adds overhead to
         every call to [f], by duplicating the code of [inter1l] and
         [inter] for the case where the arguments of [f] are flipped.
         It would be great if the compiler would create a specialized
         copy of [inter] for this situation. *)

#endif
