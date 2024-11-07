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

(* The following type definitions, macros, and code are shared between
   several kinds of trees, namely height-balanced and weight-balanced
   trees. *)

(* Each node stores its left child, key, right child, plus extra
   (balancing) information. We assume that the macro [EXTRA]
   describes these extra fields. *)

(* In the case of sets, each node carries a value [v] of type ['v].

   In the case of maps, we could use the same type definition, and later
   instantiate ['v] with the product type ['key * 'data]. However, this
   would create an indirection: every node would involve two distinct memory
   blocks. Instead, we fuse these two memory blocks into a single one: each
   node carries a key [k] and a datum [d]. OCaml's [constraint] keyword is
   used to impose an equality between the types ['v] and ['key * 'data].

   This non-standard representation requires us to construct a pair in the
   macro [ANALYZE] and to deconstruct a pair in the function [create''].
   It has no other impact. *)

#ifndef MAP_VARIANT

  type 'v tree =
    | TLeaf
    | TNode of { l : 'v tree; v : 'v;              r : 'v tree; EXTRA }

#else

  type 'v tree =
    | TLeaf
    | TNode of { l : 'v tree; k : 'key; d : 'data; r : 'v tree; EXTRA }
    constraint 'v = 'key * 'data

#endif

(* This macro destructs a tree [t].
   In case of a leaf, [case_leaf] is returned.
   In case of a node, the variables [tl], [tv], [tr] are bound.
   This macro must be followed with a semicolon;
   the code that follows this macro becomes part
   of the second branch of the [match] construct. *)

#ifndef MAP_VARIANT

  #def ANALYZE(t, case_leaf, tl, tv, tr)
    match t with
    | TLeaf ->
        case_leaf
    | TNode { l = tl; v = tv; r = tr; _ } ->
        ()
  #enddef

#else

  #def ANALYZE(t, case_leaf, tl, tv, tr)
    match t with
    | TLeaf ->
        case_leaf
    | TNode { l = tl; k = _k; d = _d; r = tr; _ } ->
        (* Construct a pair: *)
        let tv = (_k, _d) in
        ()
  #enddef

#endif

(* This function is not inlined, so as to reduce code size and produce
   more readable assembly code. *)
let impossible () =
  assert false

(* This macro destructs a tree [t] that is known not to be a leaf.
   It binds the variables [tl], [tv], [tr].
   It is intended to be followed with a semicolon. *)

#define DESTRUCT(t, tl, tv, tr) \
  ANALYZE(t, impossible(), tl, tv, tr)

(* A public view. *)

type 'v view =
  | Leaf
  | Node of 'v tree * 'v * 'v tree

let[@inline] view t =
  ANALYZE(t, Leaf, l, v, r);
  Node (l, v, r)

let leaf =
  TLeaf
