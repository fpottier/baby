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

open Signatures

(**This module defines a memory layout and a weight-based balancing scheme
   for binary search trees. It is specialized to the case where the values
   carried by the tree are pairs: see the constraint ['v = 'key * 'data]
   below. It offers an abstract API, described by the signature [BASE_MAP]. *)

(**Because (in light of the limited optimization ability of the current OCaml
   compiler) the minimal abstract interface imposes a performance penalty, we
   also expose a concrete view of the memory layout. *)
type 'v tree =
  | TLeaf
  | TNode of { l : 'v tree; k : 'key; d : 'data; r : 'v tree; w : int }
  constraint 'v = 'key * 'data

(**The base layer interface. *)
include BASE_MAP with type 'v tree := 'v tree
