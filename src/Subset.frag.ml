(******************************************************************************)
(*                                                                            *)
(*                                   Bistro                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

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
