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

(* The following code taken from OCaml's Set library, and slightly adapted. *)

let[@inline] max (x : int) (y : int) =
  if x <= y then y else x

(* In the following, some functions have nontrivial preconditions:
   [join] and its variants require [l < v < r];
   [of_sorted_unique_array_slice] requires a sorted array.
   Here, we do not have access to the ordering function [E.compare],
   so we do not write assertions to check that these preconditions
   are met. *)

(* Trees are height-balanced. Each node stores its height. The heights
   of two siblings differ by at most 2. *)

#define EXTRA h : int
#include "TreeDef.frag.ml"

(* [height t] reads and returns the height of the tree [t]. *)

let[@inline] height t =
  match t with
  | TLeaf ->
      0
  | TNode { h; _ } ->
      h

(* The weight of a tree cannot be determined in constant time. *)

let[@inline] weight _t =
  0

(* The cardinal of a tree cannot be determined in constant time. *)

(* This is a linear-time [cardinal] function. *)

let constant_time_cardinal =
  false

let rec cardinal accu t : int =
  match t with
  | TLeaf ->
      accu
  | TNode { l; r; _ } ->
      let accu = accu + 1 in
      let accu = cardinal accu l in
      cardinal accu r

let cardinal t : int =
  cardinal 0 t

(* [siblings l r] checks that [l] and [r] are siblings, that is, they
   could be siblings (the children of a binary node) in a valid tree. *)

(* [quasi_siblings l r] checks that [l] and [r] are quasi-siblings,
   that is, siblings where one tree has been disturbed by removing or
   adding one element. *)

let siblings l r =
  abs (height l - height r) <= 2

let quasi_siblings l r =
  abs (height l - height r) <= 3

(* A well-formedness check. *)

let rec check t =
  match t with
  | TLeaf ->
      ()
  | TNode { l; r; h; _ } ->
      check l;
      check r;
      assert (h = max (height l) (height r) + 1);
      assert (siblings l r)

(* [create l v r] requires [l < v < r]. It constructs a node with left child
   [l], value [v], and right child [r]. The subtrees [l] and [r] must be
   balanced, and the difference in their heights must be at most 2. *)

(* [create'' h l v r] is analogous, but requires the user to provide the
   height [h] of the new tree. *)

let[@inline] create'' h l v r =
  assert (siblings l r);
  assert (h = max (height l) (height r) + 1);
#ifndef MAP_VARIANT
  TNode { l; v; r; h }
#else
  (* Deconstruct a pair: *)
  let (k, d) = v in
  TNode { l; k; d; r; h }
#endif

let[@inline] create l v r =
  let h = max (height l) (height r) + 1 in
  create'' h l v r

(* [create] is published under the name [join_siblings]. *)

let join_siblings =
  create

(* Trees of one, two, three elements. *)

(* [doubleton x y] requires [x < y].
   [tripleton x y z] requires [x < y < z]. *)

let[@inline] singleton x =
  (* This is equivalent to [create TLeaf x TLeaf]. *)
  let h = 1 in
  create'' h TLeaf x TLeaf

let[@inline] doubleton x y =
  let h = 2 in
  create'' h TLeaf x (singleton y)

let[@inline] tripleton x y z =
  let h = 2 in
  create'' h (singleton x) y (singleton z)

(* Trees of [n] elements. *)

#include "OfSortedUniqueArraySlice.frag.ml"

(* [seems_smaller t1 t2] is equivalent to [height t1 < height t2]. *)

let[@inline] seems_smaller t1 t2 =
  match t1, t2 with
  | TLeaf, TLeaf ->
      false
  | TLeaf, _ ->
      true
  | _, TLeaf ->
      false
  | TNode { h = h1; _ }, TNode { h = h2; _ } ->
      h1 < h2

(* [bal l v r] requires [l < v < r]. It constructs a node with left child
   [l], value [v], and right child [r]. The subtrees [l] and [r] must be
   balanced, and the difference in their heights must be at most 3. If
   necessary, one step of rebalancing is performed. *)

(* Because [create] calls [height], this code involves several redundant
   computations of the height of a subtree. However, modifying the code to
   avoid this redundancy makes it much less readable and makes no measurable
   difference in the run time. *)

let bal l v r =
  assert (quasi_siblings l r);
  let hl = height l
  and hr = height r in
  if hl > hr + 2 then begin
    DESTRUCT(l, ll, lv, lr);
    if height ll >= height lr then
      create ll lv (create lr v r)
    else begin
      DESTRUCT(lr, lrl, lrv, lrr);
      create (create ll lv lrl) lrv (create lrr v r)
    end
  end
  else if hr > hl + 2 then begin
    DESTRUCT(r, rl, rv, rr);
    if height rr >= height rl then
      create (create l v rl) rv rr
    else begin
      DESTRUCT(rl, rll, rlv, rlr);
      create (create l v rll) rlv (create rlr rv rr)
    end
  end
  else
    (* This is equivalent to [create l v r]. *)
    let h = max hl hr + 1 in
    create'' h l v r

let join_quasi_siblings =
  bal

(* [add_min_element x t] requires [x < t]. It is the special case of
   [join] where the left-hand tree is empty. *)

let rec add_min_element x t =
  (* If [t] is empty, return [singleton x], otherwise bind [l, v, r]. *)
  ANALYZE(t, singleton x, l, v, r);
  (* Insert [x] into the left-hand child and reconstruct a node. *)
  bal (add_min_element x l) v r

(* [add_max_element x t] requires [t < x]. It is the special case of
   [join] where the right-hand tree is empty. *)

let rec add_max_element x t =
  ANALYZE(t, singleton x, l, v, r);
  bal l v (add_max_element x r)

(* [join l v r] requires [l < v < r]. It makes no assumptions about
   the heights of the subtrees [l] and [r]. *)

(* Sharing the code between the set and map variants by using our tree
   destruction macros, without introducing any overhead, is not so easy.
   It is easier to not use the tree destruction macros and duplicate a few
   lines of code, as follows. *)

let rec join l v r =
  match l, r with
  | TLeaf, _ ->
      add_min_element v r
  | _, TLeaf ->
      add_max_element v l
#ifndef MAP_VARIANT
  | TNode { l = ll; v = lv; r = lr; h = hl },
    TNode { l = rl; v = rv; r = rr; h = hr } ->
      if hl > hr + 2 then bal ll lv (join lr v r) else
      if hr > hl + 2 then bal (join l v rl) rv rr else
      create l v r
#else
  | TNode { l = ll; k = lk; d = ld; r = lr; h = hl },
    TNode { l = rl; k = rk; d = rd; r = rr; h = hr } ->
      if hl > hr + 2 then let lv = (lk, ld) in bal ll lv (join lr v r) else
      if hr > hl + 2 then let rv = (rk, rd) in bal (join l v rl) rv rr else
      create l v r
#endif
