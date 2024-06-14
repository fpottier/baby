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
