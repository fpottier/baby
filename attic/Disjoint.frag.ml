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

(* -------------------------------------------------------------------------- *)

(* This version of [disjoint] does not use [split], therefore does not
   construct new trees; it does not allocate memory or perform rebalancing
   work. I believe that its worst-case time complexity is not optimal. *)

(* In comparison with [disjoint] in OCaml's Set library, this version
   of [disjoint] can be twice faster and up to 40% slower. *)

(* [disjoint_node t1 l2 v2 r2] tests whether the trees [t1]
   and [NODE(l2, v2, r2)] are disjoint. *)

let rec disjoint_node t1 l2 v2 r2 =
  match VIEW(t1) with
  | LEAF ->
      true
  | NODE(l1, v1, r1) ->
      let c = E.compare v2 v1 in
      if c = 0 then
        false
      else if c < 0 then
        not (mem v1 r2) &&
        disjoint r1 r2 &&
        disjoint_node l1 l2 v2 r2
      else
        not (mem v1 l2) &&
        disjoint l1 l2 &&
        disjoint_node r1 l2 v2 r2

and disjoint (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(_, _, _), NODE(l2, v2, r2) ->
      t1 != t2 && (* fast path *)
      disjoint_node t1 l2 v2 r2

(* -------------------------------------------------------------------------- *)

(* The following versions are variations on the canonical (optimal)
   formulation of [disjoint]. *)

let rec disjoint0 (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      t1 != t2 && (* fast path *)
      if BOTH_EMPTY(l1, r1) then
        (* The tree [t1] is [singleton k1]. *)
        not (mem k1 t2)
      else if BOTH_EMPTY(l2, r2) then
        (* The tree [t2] is [singleton k2]. *)
        not (mem k2 t1)
      else
        let l1, b, r1 = split k2 t1 in
        not b && disjoint0 l1 l2 && disjoint0 r1 r2

let abandon =
  leaf, true, leaf

let rec split_absent (k : key) (t : tree) : tree * bool * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, false, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        abandon
      else if c < 0 then
        let ll, b, lr = split_absent k l in
        if b then abandon else
        ll, b, (if lr == l then t else join lr m r)
      else
        let rl, b, rr = split_absent k r in
        if b then abandon else
        (if rl == r then t else join l m rl), b, rr

let rec disjoint1 (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      t1 != t2 && (* fast path *)
      if BOTH_EMPTY(l1, r1) then
        (* The tree [t1] is [singleton k1]. *)
        not (mem k1 t2)
      else if BOTH_EMPTY(l2, r2) then
        (* The tree [t2] is [singleton k2]. *)
        not (mem k2 t1)
      else
        let l1, b, r1 = split_absent k2 t1 in
        not b && disjoint1 l1 l2 && disjoint1 r1 r2

exception NotDisjoint

let rec split13_disjoint (k : key) (t : tree) : tree * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        raise NotDisjoint
      else if c < 0 then
        let ll, lr = split13_disjoint k l in
        ll, (if lr == l then t else join lr m r)
      else
        let rl, rr = split13_disjoint k r in
        (if rl == r then t else join l m rl), rr

let rec disjoint2 (t1 : tree) (t2 : tree) : bool =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true
  | NODE(l1, k1, r1), NODE(l2, k2, r2) ->
      t1 != t2 && (* fast path *)
      if BOTH_EMPTY(l1, r1) then
        (* The tree [t1] is [singleton k1]. *)
        not (mem k1 t2)
      else if BOTH_EMPTY(l2, r2) then
        (* The tree [t2] is [singleton k2]. *)
        not (mem k2 t1)
      else
        let l1, r1 = split13_disjoint k2 t1 in
        disjoint2 l1 l2 && disjoint2 r1 r2

let disjoint2 t1 t2 =
  try
    disjoint2 t1 t2
  with NotDisjoint ->
    false

(* -------------------------------------------------------------------------- *)

(* Enumerations. *)

(* [disjoint e1 e2] determines whether the enumerations [e1] and [e2] are
   disjoint. *)

let disjoint (e1 : enum) (e2 : enum) : bool =
  match e1, e2 with
  | End, _
  | _, End ->
      true
  | More (v1, r1, e1), More (v2, r2, e2) ->
      let c = E.compare v1 v2 in
      if c = 0 then
        false
      else
        try
          if c < 0 then
            disjoint_more_more v1 r1 e1 v2 r2 e2
          else
            disjoint_more_more v2 r2 e2 v1 r1 e1
        with NotDisjoint ->
          false

(* [disjoint_et e1 t2] determines whether the enumeration [e1] and the tree
   [t2] are disjoint. It can be more efficient than [disjoint e1 (enum t2)]
   because it seeks [v1] inside [t2]. It can also be slower, because building
   [enum t2] requires no comparisons, whereas [enum_from_disjoint_1 v1 t2
   empty] does involve comparisons. *)

let disjoint_et (e1 : enum) (t2 : tree) : bool =
  match e1 with
  | End ->
      true
  | More (v1, r1, e1) ->
      try
        let e2 = enum_from_disjoint_1 v1 t2 empty in
        match e2 with
        | End ->
            true
        | More (v2, r2, e2) ->
            if debug then assert (E.compare v1 v2 < 0);
            disjoint_more_more v1 r1 e1 v2 r2 e2
      with NotDisjoint ->
        false

(* This version of [disjoint t1 t2] converts only one tree (the smaller tree)
   to an enumeration, then uses [disjoint_et]. It can be a bit faster or a bit
   slower than the more straightforward definition based on [Enum.disjoint]. *)

let disjoint t1 t2 =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      true (* fast path *)
  | _, _ ->
      t1 != t2 && (* fast path *)
      if seems_smaller t1 t2 then
        Enum.(disjoint_et (enum t1) t2)
      else
        Enum.(disjoint_et (enum t2) t1)
