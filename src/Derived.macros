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

(* Derived macros. *)

(* [EMPTY(t)] determines whether the tree [t] is empty, that is, a leaf. *)

#define EMPTY(t)        (match VIEW(t) with LEAF -> true | _ -> false)

(* [BOTH_EMPTY(l,r)] determines whether the trees [l] and [r] are both empty. *)

#define BOTH_EMPTY(l,r) (EMPTY(l) && EMPTY(r))
