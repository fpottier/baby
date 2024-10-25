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

(* Types. *)

type elt = key
type set = tree
type t = set

(* -------------------------------------------------------------------------- *)

(* Operations. *)

#include "Empty.frag.ml"
#include "MinMax.frag.ml"
#include "Mem.frag.ml"
#include "Find.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Split.frag.ml"
#include "Enum.frag.ml"
#include "Compare.frag.ml"
#include "Equal.frag.ml"
#include "Union.frag.ml"
#include "Inter.frag.ml"
#include "Diff.frag.ml"
#include "Xor.frag.ml"
#include "Disjoint.frag.ml"
#include "Subset.frag.ml"
#include "Conversions.frag.ml"
#include "Map.frag.ml"
#include "Filter.frag.ml"
#include "RandomAccess.frag.ml"
#include "Iter.frag.ml"
