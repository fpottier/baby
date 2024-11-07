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

#scope

(* -------------------------------------------------------------------------- *)

(* Types. *)

(* A key is a value of the ordered type [E.t]. *)

(* In the set variant, a set element and a key are the same thing.

   In the map variant, a map element is a binding, that is, a key-data pair.
   Therefore, a key is just the first component of a map element. *)

(* The following types exist in both variants:

   + [key] is the type of a key;
   + [DATA] is the type of a datum;
   + [ELT] is the type of an element;
   + [TREE] is the type of a tree.

   [DATA], [ELT], and [TREE] may contain a wildcard, so they can be used only
   in type annotations (as opposed to type definitions, signatures, etc.).

   The following macros exist in both variants:

   + [EXTRA(x)] expands to nothing in the set variant
                and to [x] in the map variant;
   + [GET_KEY(v)] extracts the key component of the element [v];
   + [GET_DATA(v)] extracts the data component of the element [v];
 *)

type key = E.t

#ifndef MAP_VARIANT

type elt = key
type set = elt tree
type t = set

#define DATA         elt
#define ELT          elt
#define EXTRA(x)
#define GET_KEY(v)   (v)
#define GET_DATA(v)  (v)

#else

type 'a binding = key * 'a
type 'a map = 'a binding tree
type 'a t = 'a map

#define DATA         _
#define ELT          (key * DATA)
#define EXTRA(x)     (x)
#define GET_KEY(v)   (let (_k, _) = v in _k)
#define GET_DATA(v)  (let (_, _d) = v in _d)

#endif

#define TREE        ELT tree

(* The total order on elements. *)

let[@inline] compare_elts (x : ELT) (y : ELT) : int =
  E.compare (GET_KEY(x)) (GET_KEY(y))

(* The following macro is used by [union] and [inter] on maps.
   We could also make it a function and mark it [@inline]. *)

#define COMBINE(flip, f, k, d1, d2) \
  (if flip then f k d2 d1 else f k d1 d2)

(* The following macros abstract away the distinction between
   [if] on a Boolean value and [match] on an option. *)

#ifndef MAP_VARIANT
#define IF(b) if b then
#define ELSE  else
#else
#define IF(b) match b with Some _ ->
#define ELSE  | None ->
#endif

(* The following macro applies the function [f] directly to [v] in the set
   variant and separately to the two components of the pair [v] in the map
   variant. *)

#ifndef MAP_VARIANT
#define CURRY(f, v)     ((f) (v))
#define CURRY3(f, v, s) ((f) (v) (s))
#else
#define CURRY(f, v)     (let (_k, _d) = v in f _k _d)
#define CURRY3(f, v, s) (let (_k, _d) = v in f _k _d (s))
#endif

(* -------------------------------------------------------------------------- *)

(* Operations. *)

#include "Empty.frag.ml"
#include "MinMax.frag.ml"
#include "Mem.frag.ml"
#include "Find.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Split.frag.ml"
#include "Join.frag.ml"
#include "Enum.frag.ml"
#include "Compare.frag.ml"
#include "Equal.frag.ml"
#include "Union.frag.ml"
#include "Inter.frag.ml"
#include "Diff.frag.ml"
#include "Xor.frag.ml"
#include "Disjoint.frag.ml"
#include "Subset.frag.ml"
#include "Map.frag.ml"
#include "Filter.frag.ml"
#include "Conversions.frag.ml"
#include "RandomAccess.frag.ml"
#include "Iter.frag.ml"

#ifndef MAP_VARIANT

(* In the set API, [to_list] is published under the name [elements]. *)

let elements = to_list

#else

(* Our internal function [singleton] expects a pair [(k, d)], but the
   public function [singleton] expects separate arguments [k] and [d]. *)

let[@inline] singleton k d =
  singleton (k, d)

(* In the map API, [to_list] is published under the name [bindings]. *)

let bindings = to_list

#endif

(* -------------------------------------------------------------------------- *)

#endscope
