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

(* -------------------------------------------------------------------------- *)

(* We now wrap some of the candidate functions with extra runtime checks. *)

(* We could also place these runtime checks in [Bistro] itself, under
   [if debug], so they are erased in release mode. I prefer to place
   them here. *)

(* -------------------------------------------------------------------------- *)

(* [diff] guarantees that if the result is logically equal to [t1]
   then it is physically equal to [t1]. This holds regardless of
   which balancing criterion is used. *)

let[@inline] diff t1 t2 =
  let result = diff t1 t2 in
  if equal result t1 then assert (result == t1);
  result

(* [add] and [remove] offers similar guarantees. *)

let[@inline] add x t =
  let result = add x t in
  if mem x t then assert (result == t);
  result

let[@inline] remove x t =
  let result = remove x t in
  if not (mem x t) then assert (result == t);
  result
