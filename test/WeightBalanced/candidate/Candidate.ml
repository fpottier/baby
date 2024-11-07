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

(* -------------------------------------------------------------------------- *)

(* The name of this module, viewed from outside this library. *)

let name =
  "Weight_candidate.Candidate"

(* Whether this candidate is weight-balanced. *)

let weight_balanced =
  true

(* Whether the random access functions should be tested. *)

let has_random_access_functions =
  true

(* -------------------------------------------------------------------------- *)

(* The candidate. *)

include Baby.W.Make(Int)
