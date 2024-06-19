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

(* This alternative version of [subset] does not allocate any memory.
   Unfortunately, I believe that its worst-case time complexity may
   be suboptimal, because of the calls to [mem]. In practice, I have
   observed that it can be 3x faster or 3x slower than the (optimized)
   canonical definition of [subset] that I have adopted. *)

let rec subset_node t1 l2 v2 r2 =
  match VIEW(t1) with
  | LEAF ->
      true
  | NODE(l1, v1, r1) ->
      let c = E.compare v2 v1 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        mem v1 r2 &&
        subset r1 r2 &&
        subset_node l1 l2 v2 r2
      else
        mem v1 l2 &&
        subset l1 l2 &&
        subset_node r1 l2 v2 r2

and subset (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _ ->
      true
  | _, LEAF ->
      false
  | NODE(_, _, _), NODE(l2, v2, r2) ->
      t1 == t2 || (* fast path *)
      weight t1 <= weight t2 && (* fast path *)
      subset_node t1 l2 v2 r2
