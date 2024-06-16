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

(* This file contains some experiments with intersection. *)

(* [inter7] develops the special cases where one of the subtrees [l2]
   and [r2] is empty. [inter8] further exploits the remark that the
   second argument of [inter7] is always a node. Experiments show that
   these two optimizations save at best a few percent on execution
   time and memory. *)

(* These functions exploit the following specialized versions of [split]. *)

let rec split12 (k : key) (t : tree) : tree * bool =
  match VIEW(t) with
  | LEAF ->
      leaf, false
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        l, true
      else if c < 0 then
        split12 k l
      else
        let rl, b = split12 k r in
        join l m rl, b

let rec split23 (k : key) (t : tree) : bool * tree =
  match VIEW(t) with
  | LEAF ->
      false, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        true, r
      else if c < 0 then
        let b, lr = split23 k l in
        b, join lr m r
      else
        split23 k r

(* We wish to ensure that if the result is equal to [t2]
   then the result is physically equal to [t2]. *)
let rec inter7 (t1 : tree) (t2 : tree) : tree =
  match VIEW(t1), VIEW(t2) with
  | LEAF, _
  | _, LEAF ->
      leaf
  | NODE(_, _, _), NODE(l2, k2, r2) ->
      if t1 == t2 then t2 else (* fast path *)
      match VIEW(l2), VIEW(r2) with
      | LEAF, LEAF ->
          (* The tree [t2] is [singleton k2]. *)
          if mem k2 t1 then t2 else leaf
      | _, LEAF ->
          (* [r2] is empty, so [r1] is useless. *)
          let l1, b = split12 k2 t1 in
          if b then
            let l = inter7 l1 l2 in
            if l == l2 then t2 else (* preserve sharing *)
            add k2 l
          else
            inter7 l1 l2
      | LEAF, _ ->
          (* [l2] is empty, so [l1] is useless. *)
          let b, r1 = split23 k2 t1 in
          if b then
            let r = inter7 r1 r2 in
            if r == r2 then t2 else (* preserve sharing *)
            add k2 r
          else
            inter7 r1 r2
      | _, _ ->
          (* The general case. *)
          let l1, b, r1 = split k2 t1 in
          let l = inter7 l1 l2
          and r = inter7 r1 r2 in
          if b then
            if l == l2 && r == r2 then t2 else (* preserve sharing *)
            join l k2 r
          else
            join2 l r

let inter7 t1 t2 =
  if t1 == t2 then t1 else (* fast path *)
  if seems_smaller t1 t2 then
    inter7 t2 t1
  else
    inter7 t1 t2

(* We wish to ensure that if the result is equal to [t2]
   then the result is physically equal to [t2]. *)
let rec inter8 (t1 : tree) (t2 : tree) l2 k2 r2 : tree =
  match VIEW(t1) with
  | LEAF ->
      leaf
  | NODE(_, _, _) ->
      if t1 == t2 then t2 else (* fast path *)
      match VIEW(l2), VIEW(r2) with
      | LEAF, LEAF ->
          (* The tree [t2] is [singleton k2]. *)
          if mem k2 t1 then t2 else leaf
      | NODE(l2l, l2v, l2r), LEAF ->
          (* [r2] is empty, so [r1] is useless. *)
          let l1, b = split12 k2 t1 in
          if b then
            let l = inter8 l1 l2 l2l l2v l2r in
            if l == l2 then t2 else (* preserve sharing *)
            add k2 l
          else
            inter8 l1 l2 l2l l2v l2r
      | LEAF, NODE(r2l, r2v, r2r) ->
          (* [l2] is empty, so [l1] is useless. *)
          let b, r1 = split23 k2 t1 in
          if b then
            let r = inter8 r1 r2 r2l r2v r2r in
            if r == r2 then t2 else (* preserve sharing *)
            add k2 r
          else
            inter8 r1 r2 r2l r2v r2r
      | NODE(l2l, l2v, l2r), NODE(r2l, r2v, r2r) ->
          (* The general case. *)
          let l1, b, r1 = split k2 t1 in
          let l = inter8 l1 l2 l2l l2v l2r
          and r = inter8 r1 r2 r2l r2v r2r in
          if b then
            if l == l2 && r == r2 then t2 else (* preserve sharing *)
            join l k2 r
          else
            join2 l r

let inter8 t1 t2 =
  match VIEW(t2) with
  | LEAF ->
      leaf
  | NODE(l2, k2, r2) ->
      inter8 t1 t2 l2 k2 r2

let inter8 t1 t2 =
  if t1 == t2 then t1 else (* fast path *)
  if seems_smaller t1 t2 then
    inter8 t2 t1
  else
    inter8 t1 t2
