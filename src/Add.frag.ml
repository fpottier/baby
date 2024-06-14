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

(* This is insertion in the style of BFS. *)

(* (Disabled.)

let add (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join l k r

 *)

(* This is a less elegant but more efficient version of insertion. *)

(* This implementation is taken from OCaml's Set library. *)

let rec add (x : key) (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      singleton x
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let l' = add x l in
        if l == l' then t else join_neighbors l' v r
      else
        let r' = add x r in
        if r == r' then t else join_neighbors l v r'
