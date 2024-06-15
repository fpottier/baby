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

(* The name of this module, viewed from outside this library. *)

let name =
  "Height_candidate.Candidate"

(* Whether the random access functions should be tested. *)

let has_random_access_functions =
  false

(* -------------------------------------------------------------------------- *)

(* The candidate. *)

include Bistro.H.Set.Make(Int)
