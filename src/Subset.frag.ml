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

(* Inclusion. *)

(* -------------------------------------------------------------------------- *)

(* The set variant. *)

#ifndef MAP_VARIANT

(* This simple version of [subset] has canonical structure. *)

(* (Disabled.)

let rec subset (t1 : set) (t2 : set) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODENODE(_, _, _, l2, v2, r2)
      let l1, r1 = split13 v2 t1 in
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

let rec subset (t1 : set) (t2 : set) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODENODE(l1, v1, r1, l2, v2, r2)
      t1 == t2 || (* fast path *)
      if BOTH_EMPTY(l1, r1) then
        (* The tree [t1] is [singleton v1]. *)
        mem v1 t2
      else
        weight t1 <= weight t2 && (* fast path *)
        let l1, r1 = split13 v2 t1 in
        subset l1 l2 && subset r1 r2

#else

(* -------------------------------------------------------------------------- *)

(* The map variant. *)

(* In the map variant, the name [subset] does not seem appropriate.
   I prefer to name this function [sub]. *)

(* [sub] is parameterized with a data comparison function [f]. *)

(* The physical equality tests disappear, because they impose undesirable
   type equality constraints. *)

let rec sub
  (f : 'a1 -> 'a2 -> bool)
  (t1 : 'a1 binding tree) (t2 : 'a2 binding tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODENODE(l1, v1, r1, l2, v2, r2)
      let (k1, d1) = v1 in
      if BOTH_EMPTY(l1, r1) then begin
        (* The tree [t1] is [singleton v1]. *)
        match find_opt k1 t2 with
        | None ->
            false
        | Some d2 ->
            f d1 d2
      end
      else
        weight t1 <= weight t2 && (* fast path *)
        let (k2, d2) = v2 in
        let l1, od1, r1 = split k2 t1 in
        (match od1 with None -> true | Some d1 -> f d1 d2) &&
        sub f l1 l2 && sub f r1 r2

#endif
