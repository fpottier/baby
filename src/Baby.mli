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

(**This library offers two flavors of binary search trees as well as
   building blocks that allow advanced users to construct their own
   custom flavors.

   For {b height-balanced binary search trees}, ready for use,
   please see {!Baby.H.Set.Make}.

   For {b weight-balanced binary search trees}, ready for use,
   please see {!Baby.W.Set.Make}.

 *)

(** @inline *)
include module type of Signatures

(**The module [Baby.H] provides ready-made height-balanced binary
   search trees. *)
module H : SET_MAP

(**The module [Baby.W] provides ready-made weight-balanced binary
   search trees. *)
module W : SET_MAP

(**The functor [Baby.Custom] constructs balanced binary search trees
   based on a user-supplied balancing scheme. *)
module Custom (_ : BASE_SET) (_ : BASE_MAP) : SET_MAP

(**/**)

(* The following modules must be exported, because they are (or may be) used
   in the benchmarks. Because they are somewhat unlikely to be useful to an
   end user, and because they may change in th future, their existence is not
   advertised. *)

module HeightSet : BASE_SET
module HeightMap : BASE_MAP

module WeightSet : BASE_SET
module WeightMap : BASE_MAP
