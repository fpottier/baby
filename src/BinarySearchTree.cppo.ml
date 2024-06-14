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

(* BFS = Blelloch, Ferizovic and Sun (2022). *)

open Signatures
open Profile

module[@inline] Make
(E : OrderedType)
(T : BST with type key = E.t)
= struct
include T

  #define VIEW(t)       (view t)
  #define LEAF          Leaf
  #define NODE(x, y, z) Node (x, y, z)

  #include "Common.frag.ml"

end
