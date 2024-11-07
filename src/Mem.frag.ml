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

(* The value [v] that is extracted out of the tree has type [ELT]. In the set
   variant, it is an element of the set. In the map variant, it is a pair of
   a key and a value. In either case, [GET_KEY(v)] is a key. *)

let rec mem (x : key) (t : TREE) : bool =
  match VIEW(t) with
  | LEAF ->
      false
  | NODE(l, v, r)
      let c = E.compare x (GET_KEY(v)) in
      c = 0 || mem x (if c < 0 then l else r)
