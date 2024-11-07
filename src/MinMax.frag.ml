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

let rec min_elt_1 (default : ELT) (t : TREE) : ELT =
  match VIEW(t) with
  | LEAF ->
      default
  | NODE(l, v, _)
      min_elt_1 v l

let min_elt (t : TREE) : ELT =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, _)
      min_elt_1 v l

let rec min_elt_opt_1 (default : ELT) (t : TREE) : ELT option =
  match VIEW(t) with
  | LEAF ->
      Some default
  | NODE(l, v, _)
      min_elt_opt_1 v l

let min_elt_opt (t : TREE) : ELT option =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(l, v, _)
      min_elt_opt_1 v l

let rec max_elt_1 (default : ELT) (t : TREE) : ELT =
  match VIEW(t) with
  | LEAF ->
      default
  | NODE(_, v, r)
      max_elt_1 v r

let max_elt (t : TREE) : ELT =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(_, v, r)
      max_elt_1 v r

let rec max_elt_opt_1 (default : ELT) (t : TREE) : ELT option =
  match VIEW(t) with
  | LEAF ->
      Some default
  | NODE(_, v, r)
      max_elt_opt_1 v r

let max_elt_opt (t : TREE) : ELT option =
  match VIEW(t) with
  | LEAF ->
      None
  | NODE(_, v, r)
      max_elt_opt_1 v r

(* In the map variant, the above operations have different names. *)

#ifdef MAP_VARIANT
let min_binding = min_elt
let min_binding_opt = min_elt_opt
let max_binding = max_elt
let max_binding_opt = max_elt_opt
#endif

(* As in OCaml's Set library, [choose] and [choose_opt] choose the minimum
   element of the set. This is slow (logarithmic time), but guarantees that
   [choose] respects equality: that is, if the sets [s1] and [s2] are equal
   then [choose s1] and [choose s2] are equal. *)

let choose =
  min_elt

let choose_opt =
  min_elt_opt
