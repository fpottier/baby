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

let rec split (k : key) (t : tree) : tree * bool * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, false, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        l, true, r
      else if c < 0 then
        let ll, b, lr = split k l in
        ll, b, (if lr == l then t else join lr m r)
      else
        let rl, b, rr = split k r in
        (if rl == r then t else join l m rl), b, rr

(* A specialized version of [split] that returns just the Boolean component
   of the result is [mem]. *)

(* [split13] is a variant of [split] that returns only the first and third
   components of the result. *)

let rec split13 (k : key) (t : tree) : tree * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        l, r
      else if c < 0 then
        let ll, lr = split13 k l in
        ll, (if lr == l then t else join lr m r)
      else
        let rl, rr = split13 k r in
        (if rl == r then t else join l m rl), rr

(* [join2] is known as [concat] in OCaml's Set library. *)

(* This is the code proposed by BFS. Their [split_last] function
   corresponds to our functions [min_elt] and [remove_min_elt_1].

let rec split_last (l : tree) (k : key) (r : tree) : tree * key =
  match VIEW(r) with
  | LEAF ->
      l, k
  | NODE(l', k', r') ->
      let r, m = split_last l' k' r' in
      join l k r, m

let join2 (l : tree) (r : tree) : tree =
  match VIEW(l) with
  | LEAF ->
      r
  | NODE(ll, m, lr) ->
      let l', k = split_last ll m lr in
      join l' k r

 *)

(* [join2 l r] is implemented by extracting the maximum element of [l]
   or the minimum element of [r] and letting [join] do the rest of the
   work. *)

(* In order to maintain a better balance, one might wish to extract an
   element from the tree that seems larger. However, this seems to
   bring no improvement in practice, so we avoid this complication. *)

let join2 (l : tree) (r : tree) : tree =
  match VIEW(l), VIEW(r) with
  | LEAF, _ ->
      r
  | _, LEAF ->
      l
  | _, NODE(rl, rv, rr) ->
      join
        l
        (min_elt_1 rv rl)           (* same as [min_elt r] *)
        (remove_min_elt_1 rl rv rr) (* same as [remove_min_elt r] *)
