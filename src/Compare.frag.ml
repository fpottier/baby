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

(* Comparison. *)

(* Instead of using enumerations of the trees [t1] and [t2], one could perform
   a recursive traversal of [t1], while consuming an enumeration of [t2]. I
   have benchmarked this variant: it allocates less memory, and can be faster,
   but can also be about twice slower. *)

let compare EXTRA(cmp) (t1 : TREE) (t2 : TREE) : int =
#ifndef MAP_VARIANT
  if t1 == t2 then 0 else (* fast path *)
#endif
  Enum.(compare EXTRA(cmp) (enum t1) (enum t2))
