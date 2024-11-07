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

(* This is insertion in the style of BFS. *)

(* (Disabled.) (Set variant only.)

let add (k : key) (t : TREE) : TREE =
  let l, _, r = split k t in
  join l k r

 *)

#ifndef MAP_VARIANT

(* This is a less elegant but more efficient version of insertion. *)

(* This implementation is adapted from OCaml's Set library. *)

(* If an equivalent element is already present in the set, then the
   existing element is retained, so the set is unchanged. *)

let rec add (x : key) (t : TREE) : TREE =
  match VIEW(t) with
  | LEAF ->
      singleton x
  | NODE(l, v, r)
      let c = E.compare x v in
      if c = 0 then
        (* Retain the existing element: *)
        t
      else if c < 0 then
        let l' = add x l in
        if l == l' then t else join_quasi_siblings l' v r
      else
        let r' = add x r in
        if r == r' then t else join_quasi_siblings l v r'

(* [add_absent] is currently used by [xor]. We could optimize it,
   a little, by removing one test. *)

let add_absent = add

#else

(* If a binding for this key is already present in the map, then this
   existing binding is replaced with a new binding. *)

(* This implementation is adapted from OCaml's Map library. *)

let rec replace (x : key) (d : DATA) (t : TREE) : TREE =
  match VIEW(t) with
  | LEAF ->
      let v = (x, d) in
      singleton v
  | NODE(l, v, r)
      let c = E.compare x (GET_KEY(v)) in
      if c = 0 then
        (* Overwrite the existing element: *)
        if GET_DATA(v) == d then t else
        let v = (x, d) in
        join_siblings l v r
      else if c < 0 then
        let l' = replace x d l in
        if l == l' then t else join_quasi_siblings l' v r
      else
        let r' = replace x d r in
        if r == r' then t else join_quasi_siblings l v r'

(* [replace] is published under the name [add], but one should be
   aware that [add] and [replace] are really two distinct functions,
   as they differ in the way they choose between the existing element
   and the new element. *)

let add = replace

(* See also [update] and [union1]. *)

(* [add_absent] is currently used by [xor]. It takes a key-data pair. *)

let add_absent (x, d) t =
  add x d t

#endif
