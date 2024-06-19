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

(* [map] is defined in the same way as in OCaml's Set library. *)

(* [tree_below_key] and [key_below_tree] invoke [min_elt] or [max_elt],
   whose cost is the height of the subtree. The cumulative cost of
   these calls, during the execution of [map], is of the form
   1 * n/2 + 2 * n/4 + 3 * n/8 + ..., that is, O(n). *)

(* If the function [f] is monotone, then the tests in [lax_join]
   always succeed, so [join] is invoked at every node, and every
   such call runs in constant time, since no rebalancing is
   required. Thus, in this case, [map] runs in linear time. *)

(* Otherwise, I believe (but have not carefully checked) that the
   complexity of [map] is O(n.log n). *)

let[@inline] tree_below_key (t : tree) (x : key) : bool =
  match VIEW(t) with
  | LEAF ->
      true
  | NODE(_, v, r) ->
      E.compare (max_elt_1 v r) x < 0

let[@inline] key_below_tree (x : key) (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      true
  | NODE(l, v, _) ->
      E.compare x (min_elt_1 v l) < 0

(* [lax_join l v r] is analogous to [join l v r], but does not
   require [l < v < r]. *)

let[@inline] lax_join l v r =
  if tree_below_key l v && key_below_tree v r then
    join l v r
  else
    union l (add v r)

let rec map f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      leaf
  | NODE(l, v, r) ->
     (* Enforce left-to-right evaluation order. *)
     let l' = map f l in
     let v' = f v in
     let r' = map f r in
     if l == l' && v == v' && r == r' then t (* preserve sharing *)
     else lax_join l' v' r'

(* -------------------------------------------------------------------------- *)

(* [lax_join2] plays the role of [try_concat] in OCaml's Set library,
   but is implemented in a slightly better way. *)

let lax_join2 t1 t2 =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      t2
  | _, LEAF ->
      t1
  | _, _ ->
      if E.compare (max_elt t1) (min_elt t2) < 0 then
        join2 t1 t2
      else
        union t1 t2

(* [filter_map] is defined in the same way as in OCaml's Set library. *)

let rec filter_map f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      leaf
  | NODE(l, v, r) ->
     (* Enforce left-to-right evaluation order. *)
     let l' = filter_map f l in
     let v' = f v in
     let r' = filter_map f r in
     match v' with
     | Some v' ->
         if l == l' && v == v' && r == r' then t (* preserve sharing *)
         else lax_join l' v' r'
     | None ->
         lax_join2 l' r'
