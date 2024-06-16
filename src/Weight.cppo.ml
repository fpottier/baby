(**************************************************************************)
(*                                                                        *)
(*                                  Bistro                                *)
(*                                                                        *)
(*                      François Pottier, Inria Paris                     *)
(*                                                                        *)
(*      Copyright 2024--2024 Inria. All rights reserved. This file is     *)
(*      distributed under the terms of the GNU Library General Public     *)
(*      License, with an exception, as described in the file LICENSE.     *)
(*                                                                        *)
(**************************************************************************)

open Signatures

(* This code implements weight-balancing, following Blelloch, Ferizovic and
   Sun (2022), thereafter abbreviated as BFS. The balancing invariant and
   algorithm are the same as in the paper, but this code is more aggressively
   optimized; some redundant tests and memory allocations are avoided. *)

(* This function is not inlined, so as to reduce code size and produce
   more readable assembly code. *)
let impossible () =
  assert false

(* Although this functor requires an ordered type, the ordering function
   [E.compare] is used in assertions only. *)
module[@inline] Make (E : OrderedType) = struct

  type key = E.t

  (* Trees are weight-balanced. Each node stores its left child, key, right
     child, and weight. *)

  (* The weight of a tree is the number of its leaves, that is, ONE MORE
     than the number of its nodes. The weight of a tree is never zero. *)

  (* Thus, the weight of a leaf is 1, and the weight of a node is the sum
     of the weights of its children. *)

  type tree =
    | TLeaf
    | TNode of { l : tree; v : key; r : tree; w : int }

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

  let[@inline] cardinal (t : tree) : int =
    weight t - 1

  (* This macro destructs a tree [t] that is known not to be a leaf.
     It binds the variables [tl], [tv], [tr].
     It is intended to be followed with a semicolon. *)

  #define DESTRUCT(t,tl,tv,tr) \
    match t with \
    | TLeaf -> impossible() \
    | TNode { l = tl; v = tv; r = tr; _ } -> \
        ()

  (* This macro destructs a tree [t] that is known not to be a leaf
     and whose weight is [w].
     It binds the variables [wtl], [tl], [tv], [wtr], [tr].
     It is intended to be followed with a semicolon. *)

  #define DESTRUCTW(w,t,wtl,tl,tv,wtr,tr) \
    DESTRUCT(t, tl, tv, tr); \
    let wtl = weight tl in \
    let wtr = w - wtl in \
    assert (wtr = weight tr)

  (* Weight-balanced trees with parameter α maintain the following invariant:
     for every tree [t] whose children are [l] and [r],

                    α  ≤  weight(l) / weight(t)  ≤  1 - α
                    α  ≤  weight(r) / weight(t)  ≤  1 - α

     Note that weight(l) + weight(r) is weight(t), so the two lines are
     equivalent.

     The trees [l] and [r] have like weights (can be siblings) if these
     inequalities hold. According to BFS, if 2/11 < α < 1-1/sqrt(2) holds,
     then [join] can be implemented using just single and double rotations.
     This translates to 0.(18.) < α < 0.2928932... BFS use the value 0.29 in
     their experiments. *)

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
    TNode { l; v; r; w }

  let[@inline] create l v r =
    let w = weight l + weight r in
    create'' w l v r

  let[@inline] create' wl l v wr r =
    assert (wl = weight l && wr = weight r);
    let w = wl + wr in
    create'' w l v r

  (* [create] is published under the name [join_weight_balanced]. *)

  let join_weight_balanced l v r =
    assert (siblings l r);
    create l v r

  (* Trees of one, two, three elements. *)

  let[@inline] singleton x =
    (* This is equivalent to [create TLeaf x TLeaf]. *)
    TNode { l = TLeaf; v = x; r = TLeaf; w = 2 }

  let[@inline] doubleton x y =
    assert (E.compare x y < 0);
    TNode { l = TLeaf; v = x; r = singleton y; w = 3 }

  let[@inline] tripleton x y z =
    assert (E.compare x y < 0);
    assert (E.compare y z < 0);
    TNode { l = singleton x; v = y; r = singleton z; w = 4 }

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
    else
      (* [rotate_left l v (rotate_right rl rv rr)] *)
      DESTRUCTW(wrl, rl, wrll, rll, rlv, wrlr, rlr);
      let w = wl + wr in
      create'' w (create' wl l v wrll rll) rlv (create' wrlr rlr rv wrr rr)

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
    else
      (* [rotate_right (rotate_left ll lv lr) v r] *)
      DESTRUCTW(wlr, lr, wlrl, lrl, lrv, wlrr, lrr);
      let w = wl + wr in
      create'' w (create' wll ll lv wlrl lrl) lrv (create' wlrr lrr v wr r)

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

  (* [join_neighbors l v r] requires [l < v < r]. It assumes that the weights
     of the subtrees [l] and [r] are close enough so that at most one step of
     rebalancing suffices. (The exact precondition is unclear.) *)

  let join_neighbors l v r =
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

  (* A public view. *)

  type view =
    | Leaf
    | Node of tree * key * tree

  let[@inline] view t =
    match t with
    | TLeaf ->
        Leaf
    | TNode { l; v; r; _ } ->
        Node (l, v, r)

  let leaf =
    TLeaf

end
