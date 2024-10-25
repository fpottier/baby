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

(**This module defines a memory layout and a balancing scheme for
   height-balanced binary search trees. *)

(**Because (in light of the limited optimization ability of the current OCaml
   compiler) the minimal abstract interface imposes a performance penalty, we
   also expose a concrete view of the memory layout. *)
type 'key tree =
  | TLeaf
  | TNode of { l : 'key tree; v : 'key; r : 'key tree; h : int }

(**The minimal abstract interface. *)
include CORE with type 'key tree := 'key tree
