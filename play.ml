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

(* dune utop *)
(* #use "play.ml" *)

(* @03: Failure in an instruction. *)
          #require "monolith";;
          module Sup = Monolith.Support;;
          open Baby.WeightBalanced.Make(Int);;
(* @01 *) let x0 =
            of_sorted_unique_array
            [| (-15); (-13); (-12); (-8); (-7); (-4); (-2); (-1); 1; 2; 13 |];;
(* @02 *) let x1 = of_array [| 8; (-10) |];;
(* @03 *) let _ = xor x0 x1;;
(* @03: The exception File "Weight.cppo.ml", line 221, characters 18-24: Assertion failed was raised. *)
