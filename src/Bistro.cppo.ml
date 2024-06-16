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

include Signatures

(* -------------------------------------------------------------------------- *)

(* The functor [Bistro.Make] constructs balanced binary search trees
   based on a user-supplied balancing scheme. *)

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

(* -------------------------------------------------------------------------- *)

(* The module [Bistro.H] provides ready-made height-balanced binary
   search trees. *)

module H = struct

  module Set = HeightBalanced

end

(* -------------------------------------------------------------------------- *)

(* The module [Bistro.W] provides ready-made weight-balanced binary
   search trees. *)

module W = struct

  module Set = WeightBalanced

end

(* -------------------------------------------------------------------------- *)

(* The following modules must be exported, because they are (or may be) used
   in the benchmarks. Because they are somewhat unlikely to be useful to an
   end user, their existence is not advertised. *)

module Height = Height
module Weight = Weight
