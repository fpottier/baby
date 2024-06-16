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

(* Our implementation of [union] is in the same style as [inter].
   It inherits two features of OCaml's Set library:
   - the tree that seems smaller is split;
   - if a subtree is a singleton then [union] degenerates to [add].
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
      if BOTH_EMPTY(l2, r2) then add k2 t1 else
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
