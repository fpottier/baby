(* dune utop *)
(* #use "play.ml" *)

(* @03: Failure in an instruction. *)
          #require "monolith";;
          module Sup = Monolith.Support;;
          open Bbst.WeightBalanced.Make(Int);;
(* @01 *) let x0 =
            of_sorted_unique_array
            [| (-15); (-13); (-12); (-8); (-7); (-4); (-2); (-1); 1; 2; 13 |];;
(* @02 *) let x1 = of_array [| 8; (-10) |];;
(* @03 *) let _ = xor x0 x1;;
(* @03: The exception File "Weight.cppo.ml", line 221, characters 18-24: Assertion failed was raised. *)
