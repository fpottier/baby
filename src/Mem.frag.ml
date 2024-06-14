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

(* Membership. *)

let rec mem (x : key) (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      false
  | NODE(l, v, r) ->
      let c = E.compare x v in
      c = 0 || mem x (if c < 0 then l else r)
