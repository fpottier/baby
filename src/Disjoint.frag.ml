(**************************************************************************)
(*                                                                        *)
(*                                  Bistro                                *)
(*                                                                        *)
(*                      François Pottier, Inria Paris                     *)
(*                                                                        *)
(*      Copyright 2024--2024 Inria. All rights reserved. This file is     *)
(*      distributed under the terms of the GNU Library General Public     *)
(*      License, with an exception, as described in the file LICENSE.     *)
(*                                                                        *)
(**************************************************************************)

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
