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

(* -------------------------------------------------------------------------- *)

(* Difference. *)

(* This is a simple, elegant version of [diff]. This version splits the
   tree [t1]. (Set variant only.)

let rec diff (t1 : TREE) (t2 : TREE) : TREE =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      leaf
  | _, LEAF ->
      t1
  | NODENODE(_, _, _, l2, k2, r2)
      let l1, r1 = split13 k2 t1 in
      let l = diff l1 l2
      and r = diff r1 r2 in
      join2 l r

 *)

(* This version of [diff] guarantees that if the result is equal to [t1]
   then [t1] itself is returned. It splits the tree [t2]. *)

let rec diff (t1 : TREE) (t2 : TREE) : TREE =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      leaf
  | _, LEAF ->
      t1
  | NODENODE(l1, v1, r1, l2, v2, r2)
      let k1 = GET_KEY(v1) in
#ifndef MAP_VARIANT
      if t1 == t2 then leaf else (* fast path *)
#endif
      if BOTH_EMPTY(l1, r1) then
        (* [t1] is [singleton v1]. *)
        if mem k1 t2 then leaf else t1
      else if BOTH_EMPTY(l2, r2) then
        (* [t2] is [singleton v2]. *)
        let k2 = GET_KEY(v2) in
        remove k2 t1
      else
        let l2, b, r2 = split k1 t2 in
        let l = diff l1 l2
        and r = diff r1 r2 in
        IF(b)
          join2 l r
        ELSE
          if l == l1 && r == r1 then t1 else (* preserve sharing *)
          join l v1 r
