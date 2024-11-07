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

(* [split] is implemented in the same way in OCaml's Set library and by BFS. *)

(* We use the same code, but add a physical equality test that allows us to
   preserve sharing (and avoid memory allocation) in some cases. *)

(* In the set variant, the second component of the result has type [bool].
   In the map variant, it has type [DATA option]. *)

#scope

#ifndef MAP_VARIANT
  #define RESULT     bool
  #define ABSENT     false
  #define PRESENT    true
#else
  #define RESULT     DATA option
  #define ABSENT     None
  #define PRESENT    Some (GET_DATA(v))
#endif

let rec split (k : key) (t : TREE) : TREE * RESULT * TREE =
  match VIEW(t) with
  | LEAF ->
      leaf, ABSENT, leaf
  | NODE(l, v, r)
      let c = E.compare k (GET_KEY(v)) in
      if c = 0 then
        l, PRESENT, r
      else if c < 0 then
        let ll, b, lr = split k l in
        ll, b, (if lr == l then t else join lr v r)
      else
        let rl, b, rr = split k r in
        (if rl == r then t else join l v rl), b, rr

#endscope

#ifndef MAP_VARIANT

(* A specialized version of [split] that returns just the Boolean component
   of the result is [mem]. *)

(* [split13] is a variant of [split] that returns only the first and third
   components of the result. *)

let rec split13 (k : key) (t : 'a tree) : 'a tree * 'a tree =
  match VIEW(t) with
  | LEAF ->
      leaf, leaf
  | NODE(l, v, r)
      let c = E.compare k v in
      if c = 0 then
        l, r
      else if c < 0 then
        let ll, lr = split13 k l in
        ll, (if lr == l then t else join lr v r)
      else
        let rl, rr = split13 k r in
        (if rl == r then t else join l v rl), rr

#endif
