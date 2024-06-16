(******************************************************************************)
(*                                                                            *)
(*                                   Bistro                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* [map_to_array_slice t a i] writes the elements of the tree [t],
   transformed by the function [f], to the array slice determined by
   the array [a] and the start index [i]. It returns the end index of
   this slice. *)

let rec map_to_array_slice f (t : tree) a i : int =
  if debug then assert (0 <= i && i + cardinal t <= Array.length a);
  match VIEW(t) with
  | LEAF ->
      i
  | NODE(l, v, r) ->
      let i = map_to_array_slice f l a i in
      a.(i) <- f v;
      let i = i + 1 in
      map_to_array_slice f r a i

(* [map_to_array f t] writes the elements of the tree [t],
   transformed by the function [f], to a fresh array. *)

let map_to_array f (t : tree) : key array =
  match VIEW(t) with
  | LEAF ->
      [||]
  | NODE(_, dummy, _) ->
      let n = cardinal t in
      let a = Array.make n dummy in
      let j = map_to_array_slice f t a 0 in
      if debug then assert (n = j);
      a

(* This implementation of [map] writes the transformed data to an array,
   then builds a tree out of this array. It can be marginally faster than
   the [map] function of OCaml's Set library, in the scenario where the
   function [f] scrambles the data. However, it does not preserve sharing,
   so its semantics is observably different. *)

let map f (t : tree) =
  of_array (map_to_array f t)
