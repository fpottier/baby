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

(* This is the type of the functor [Set.Make]. *)
module type FACTORY =
  functor (E : Set.OrderedType) -> Set.S with type elt = E.t

(* We check that our functors have the same type. This guarantees that (as far
   as the type-checker is concerned) they can be used as drop-in replacements
   for OCaml's [Set.Make]. *)
module TestH = (Baby.H.Set.Make : FACTORY)
module TestW = (Baby.W.Set.Make : FACTORY)
