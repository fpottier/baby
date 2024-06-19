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

let rec find (x : key) (t : tree) : key =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        v
      else if c < 0 then
        find x l
      else
        find x r

let rec find_opt (x : key) (t : tree) : key option =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        Some v
      else if c < 0 then
        find_opt x l
      else
        find_opt x r

(* -------------------------------------------------------------------------- *)

(* [find_first] and its variants are as in OCaml's Set library. *)

(* A lot of repetitive code. *)

(* It is worth noting that [find_first] is not a naive linear search.
   Instead, it assumes that [f] is a monotonically increasing function
   of elements to Booleans. This implies that there is at most one
   position in the increasing sequence of the set elements where the
   value of [f] changes, and it changes from [false] to [true]. This
   position can be found in logarithmic time. *)

let rec find_first_aux v0 f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      v0
  | NODE(l, v, r) ->
      if f v then
        find_first_aux v f l
      else
        find_first_aux v0 f r

let rec find_first f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r) ->
      if f v then
        find_first_aux v f l
      else
        find_first f r

let rec find_first_opt_aux v0 f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      Some v0
  | NODE(l, v, r) ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt_aux v0 f r

let rec find_first_opt f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(l, v, r) ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt f r

let rec find_last_aux v0 f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      v0
  | NODE(l, v, r) ->
      if f v then
        find_last_aux v f r
      else
        find_last_aux v0 f l

let rec find_last f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r) ->
      if f v then
        find_last_aux v f r
      else
        find_last f l

let rec find_last_opt_aux v0 f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      Some v0
  | NODE(l, v, r) ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt_aux v0 f l

let rec find_last_opt f (t : tree) =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(l, v, r) ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt f l
