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

(**This module defines a memory layout and a height-based balancing scheme for
   binary search trees. It offers an abstract API, described by the signature
   [BASE_SET]. *)

(**Because (in light of the limited optimization ability of the current OCaml
   compiler) the minimal abstract interface imposes a performance penalty, we
   also expose a concrete view of the memory layout. *)
type 'v tree =
  | TLeaf
  | TNode of { l : 'v tree; v : 'v; r : 'v tree; h : int }

(**The base layer interface. *)
include BASE_SET with type 'v tree := 'v tree
