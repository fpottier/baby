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

(* Equality. *)

(* Equality of sets can be implemented in several ways. E.g., [equal t1 t2]
   could be implemented in one line by [subset t1 t2 && subset t2 t1] or also
   in one line by [is_empty (xor t1 t2)]. (The latter idea could be optimized,
   so as to avoid actually constructing the tree [xor t1 t2] in memory.) Some
   experiments suggest that either of these approaches is more expensive than
   the following approach, which compares two enumerations. Furthermore, this
   approach works for both sets and maps. *)

(* In weight-balanced trees, the weight of a tree can be determined in
   constant time. This yields a fast path: if the weights and [t1] and [t2]
   differ, then they cannot possibly be equal. In height-balanced trees, the
   [weight] function returns a constant value, so this fast path is
   disabled. *)

let equal EXTRA(eq) t1 t2 =
#ifndef MAP_VARIANT
  t1 == t2 || (* fast path *)
#endif
  weight t1 = weight t2 && (* fast path *)
  Enum.(equal EXTRA(eq) (enum t1) (enum t2))
