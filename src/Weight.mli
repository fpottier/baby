(******************************************************************************)
(*                                                                            *)
(*                                   Bistro                                   *)
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
module Make (E : OrderedType)
: sig

  type key = E.t

  (**Because (in light of the limited optimization ability of the current OCaml
     compiler) the minimal abstract interface imposes a performance penalty, we
     also expose a concrete view of the memory layout. *)
  type tree =
    | TLeaf
    | TNode of { l : tree; v : key; r : tree; w : int }

  (**The minimal abstract interface. *)
  include BST with type key := E.t
               and type tree := tree

end
