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

(* This code implements weight-balancing, following Blelloch, Ferizovic and
   Sun (2022), thereafter abbreviated as BFS. The balancing invariant and
   algorithm are the same as in the paper, but this code is more aggressively
   optimized; some redundant tests and memory allocations are avoided. *)

(* In the following, some functions have nontrivial preconditions:
   [join] and its variants require [l < v < r];
   [of_sorted_unique_array_slice] requires a sorted array.
   Here, we do not have access to the ordering function [E.compare],
   so we do not write assertions to check that these preconditions
   are met. *)

(* Trees are weight-balanced. Each node stores its weight. *)

(* The weight of a tree is the number of its leaves, that is, ONE MORE
   than the number of its nodes. The weight of a tree is never zero. *)

(* Thus, the weight of a leaf is 1, and the weight of a node is the sum
   of the weights of its children. *)

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

#define EXTRA w : int
#include "TreeDef.frag.ml"

(* [weight t] reads and returns the weight of the tree [t]. *)

let[@inline] weight t =
  match t with
  | TLeaf ->
      1
  | TNode { w; _ } ->
      w

(* Weight-balanced trees offer a constant time [cardinal] function. *)

let constant_time_cardinal =
  true

let[@inline] cardinal t =
  weight t - 1

(* This macro destructs a tree [t] that is known not to be a leaf
   and whose weight is [w].
   It binds the variables [wtl], [tl], [tv], [wtr], [tr].
   It is intended to be followed with a semicolon. *)

#def DESTRUCTW(w,t,wtl,tl,tv,wtr,tr)
  DESTRUCT(t, tl, tv, tr);
  let wtl = weight tl in
  let wtr = w - wtl in
  assert (wtr = weight tr)
#enddef

(* Weight-balanced trees with parameter α maintain the following invariant:
   for every tree [t] whose children are [l] and [r],

                  α  ≤  weight(l) / weight(t)  ≤  1 - α
                  α  ≤  weight(r) / weight(t)  ≤  1 - α

   Note that weight(l) + weight(r) is weight(t), so the two lines are
   equivalent.

   The trees [l] and [r] have like weights (can be siblings) if these
   inequalities hold. According to BFS, if α < 1-1/sqrt(2) holds, then
   [join] can be implemented, using just single and double rotations.
   This translates to α < 0.2928932... BFS use the value 0.29 in their
   experiments.

   The literature also mentions the constraint 2/11 < α, that is,
   0.(18.) < α. According to BFS, this constraint is *NOT* required for
   the correctness or complexity of [join]. It *IS* however required to
   guarantee the correctness of [join_quasi_siblings], that is, to guarantee
   that if the weight of a subtree is off by one, then one (single or
   double) rotation at the root suffices. These two claims are confirmed
   by our tests. *)

let alpha =
  29 (* in percent *)

(* If [α * weight(l) <= (1-α) * weight(r)] holds, then the weight of the
   subtree [l] is acceptable; in other words, the tree [NODE(l, v, r)]
   is not left heavy. *)

let[@inline] not_left_heavy wl wr =
  alpha * wl <= (100-alpha) * wr

let[@inline] left_heavy wl wr =
  not (not_left_heavy wl wr)

(* Symmetrically, if [α * weight(r) <= (1-α) * weight(l)] holds, then the
   the tree [NODE(l, v, r)] is not right heavy. *)

let[@inline] not_right_heavy wl wr =
  not_left_heavy wr wl

let[@inline] right_heavy wl wr =
  not (not_right_heavy wl wr)

(* If both inequalities hold, then the subtrees [l] and [r] have like
   weights. This means that they can be siblings in a valid tree. *)

let[@inline] like_weights wl wr =
  not_left_heavy wl wr && not_right_heavy wl wr

let[@inline] siblings l r =
  like_weights (weight l) (weight r)

(* A well-formedness check. *)

let rec check t =
  match t with
  | TLeaf ->
      ()
  | TNode { l; r; w; _ } ->
      check l;
      check r;
      assert (w = weight l + weight r);
      assert (siblings l r)

(* [create l v r] requires [l < v < r]. It constructs a node with left
   child [l], value [v], and right child [r]. The subtrees [l] and [r]
   must be balanced and must have like weights. *)

(* [create'' w l v r] is analogous, but requires the user to provide the
   weight [w] of the new tree. *)

(* [create' wl l v wr r] is analogous, but requires the user to provide
   the weights [wl] and [wr] of the trees [l] and [r]. *)

let[@inline] create'' w l v r =
  assert (w = weight l + weight r);
  assert (siblings l r);
#ifndef MAP_VARIANT
  TNode { l; v; r; w }
#else
  (* Deconstruct a pair: *)
  let (k, d) = v in
  TNode { l; k; d; r; w }
#endif

let[@inline] create l v r =
  let w = weight l + weight r in
  create'' w l v r

let[@inline] create' wl l v wr r =
  assert (wl = weight l && wr = weight r);
  let w = wl + wr in
  create'' w l v r

(* [create] is published under the name [join_siblings]. *)

let join_siblings =
  create

(* Trees of one, two, three elements. *)

(* [doubleton x y] requires [x < y].
   [tripleton x y z] requires [x < y < z]. *)

let[@inline] singleton x =
  (* This is equivalent to [create TLeaf x TLeaf]. *)
  let w = 2 in
  create'' w TLeaf x TLeaf

let[@inline] doubleton x y =
  let w = 3 in
  create'' w TLeaf x (singleton y)

let[@inline] tripleton x y z =
  let w = 4 in
  create'' w (singleton x) y (singleton z)

(* Trees of [n] elements. *)

#include "OfSortedUniqueArraySlice.frag.ml"

(* [seems_smaller t1 t2] is equivalent to [weight t1 < weight t2]. *)

let[@inline] seems_smaller t1 t2 =
  match t1, t2 with
  | TLeaf, TLeaf ->
      false
  | TLeaf, _ ->
      true
  | _, TLeaf ->
      false
  | TNode { w = w1; _ }, TNode { w = w2; _ } ->
      w1 < w2

(* The following functions are unused, because they have been manually
   inlined and optimized. They are kept for reference. *)

(* Left and right rotations. *)

(* [rotate_left l v r] requires [l < v < r]. It applies a left rotation to
   the node [TNode { l; v; r }], without actually allocating this node. *)

(* The subtree [r] must be nonempty: it is decomposed into [rl < rv < rr].
   Then, the new tree [create (create l v rl) rv rr] is constructed. For
   this new tree to be well-formed, [l] and [rl] must have like weights, and
   the combined weights of [l] and [rl] must be like the weight of [rr]. *)

let _rotate_left l v r =
  DESTRUCT(r, rl, rv, rr);
  create (create l v rl) rv rr

let _rotate_right l v r =
  DESTRUCT(l, ll, lv, lr);
  create ll lv (create lr v r)

(* Double rotations. *)

(* [rotate_left l v (rotate_right rl rv rr)] *)

let _rotate_left_rotate_right l v rl rv rr =
  DESTRUCT(rl, rll, rlv, rlr);
  create (create l v rll) rlv (create rlr rv rr)

(* [rotate_right (rotate_left ll lv lr) v r] *)

let _rotate_right_rotate_left ll lv lr v r =
  DESTRUCT(lr, lrl, lrv, lrr);
  create (create ll lv lrl) lrv (create lrr v r)

(* [balance_right_heavy l v r] expects the tree [NODE(l, v, r)] to be right
   heavy. It addresses this problem by performing a rotation or a double
   rotation. *)

(* The choice of the parameter [alpha] is supposed to ensure that this is
   enough to re-establish the balancing invariant. However, I have not seen
   the proof, so the exact precondition of [balance_right_heavy] is unclear
   to me. Presumably the subtree [r] must not be excessively heavy. *)

let balance_right_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (right_heavy wl wr);
  DESTRUCTW(wr, r, wrl, rl, rv, wrr, rr);
  if like_weights wl wrl && like_weights (wl + wrl) wrr then
    (* [rotate_left l v r] *)
    let w = wl + wr in
    create'' w (create' wl l v wrl rl) rv rr
  else begin
    (* [rotate_left l v (rotate_right rl rv rr)] *)
    DESTRUCTW(wrl, rl, wrll, rll, rlv, wrlr, rlr);
    let w = wl + wr in
    create'' w (create' wl l v wrll rll) rlv (create' wrlr rlr rv wrr rr)
  end

(* [balance_maybe_right_heavy l v r] expects the tree [NODE(l, v, r)] to be
   either balanced or right heavy. *)

let[@inline] balance_maybe_right_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (not_left_heavy wl wr);
  if not_right_heavy wl wr then
    create' wl l v wr r
  else
    balance_right_heavy wl l v wr r

(* The following two functions are symmetric with the previous two. *)

let balance_left_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (left_heavy wl wr);
  DESTRUCTW(wl, l, wll, ll, lv, wlr, lr);
  if like_weights wlr wr && like_weights wll (wlr + wr) then
    (* [rotate_right l v r] *)
    let w = wl + wr in
    create'' w ll lv (create' wlr lr v wr r)
  else begin
    (* [rotate_right (rotate_left ll lv lr) v r] *)
    DESTRUCTW(wlr, lr, wlrl, lrl, lrv, wlrr, lrr);
    let w = wl + wr in
    create'' w (create' wll ll lv wlrl lrl) lrv (create' wlrr lrr v wr r)
  end

let[@inline] balance_maybe_left_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (not_right_heavy wl wr);
  if not_left_heavy wl wr then
    create' wl l v wr r
  else
    balance_left_heavy wl l v wr r

(* [join_maybe_left_heavy l v wr r] requires [l < v < r]. It assumes that
   the trees [l] and [r] have like weights OR that the tree [l] is heavier.
   In other words, it assumes that [NODE(l, v, r)] may be left heavy, but
   is not right heavy. *)

(* [join_maybe_left_heavy] corresponds to [joinRightWB] in BFS, Figure 8. *)

(* In this recursive function, the parameter [r] is invariant: the right
   branch of the tree [l] is followed until a node with like weight to [r]
   is reached. Then, on the way back, rebalancing is performed by invoking
   [balance_maybe_right_heavy]. *)

let rec join_maybe_left_heavy l v wr r =
  assert (wr = weight r);
  let wl = weight l in
  assert (not_right_heavy wl wr);
  if not_left_heavy wl wr then
    create' wl l v wr r
  else
    join_left_heavy wl l v wr r

(* [join_left_heavy] assumes that [NODE(l, v, r)] is left heavy. *)

and join_left_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (left_heavy wl wr);
  DESTRUCTW(wl, l, wll, ll, lv, wlr, lr);
  balance_maybe_right_heavy
    wll ll
    lv
    (wlr + wr) (join_maybe_left_heavy lr v wr r)

(* The following two functions are symmetric with the previous two. *)

let rec join_maybe_right_heavy wl l v r =
  assert (wl = weight l);
  let wr = weight r in
  assert (not_left_heavy wl wr);
  if not_right_heavy wl wr then
    create' wl l v wr r
  else
    join_right_heavy wl l v wr r

and join_right_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (right_heavy wl wr);
  DESTRUCTW(wr, r, wrl, rl, rv, wrr, rr);
  balance_maybe_left_heavy
    (wl + wrl) (join_maybe_right_heavy wl l v rl)
    rv
    wrr rr

(* [join l v r] requires [l < v < r]. It makes no assumptions about the
   weights of the subtrees [l] and [r]. *)

let join l v r =
  let wl = weight l and wr = weight r in
  if not_left_heavy wl wr then
    if not_right_heavy wl wr then
      (* balanced *)
      create' wl l v wr r
    else
      (* right heavy *)
      join_right_heavy wl l v wr r
  else
    (* left heavy *)
    join_left_heavy wl l v wr r

(* [quasi_siblings l r] checks that [l] and [r] are quasi-siblings,
   that is, siblings where one tree has been disturbed by removing or
   adding one element. *)

(* Unfortunately, I do not have a more pleasing definition of this
   function. This definition is naive; perhaps it can be simplified.
   Anyway, this function is used only in one assertion (below), so
   this problem is not serious. *)

let rec quasi_siblings l r =
  if weight l <= weight r then
    like_weights (weight l) (weight r - 1) ||
    like_weights (weight l + 1) (weight r)
  else
    quasi_siblings r l

(* [join_quasi_siblings l v r] requires [l < v < r]. It also requires
   the trees [l] and [r] to be quasi-siblings. This ensures that the
   weights of [l] and [r] are close enough so that at most one step of
   rebalancing suffices. *)

let join_quasi_siblings l v r =
  assert (quasi_siblings l r);
  let wl = weight l and wr = weight r in
  if not_left_heavy wl wr then
    if not_right_heavy wl wr then
      (* balanced *)
      create' wl l v wr r
    else
      (* right heavy *)
      balance_right_heavy wl l v wr r
  else
    (* left heavy *)
    balance_left_heavy wl l v wr r
