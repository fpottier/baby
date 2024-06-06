open Printf
open Signatures
open Profile

let verbose =
  false

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

  (* This macro destructs a tree [t] that is known not to be a leaf. *)

  #define DESTRUCT(t,tl,tv,tr) \
    match t with \
    | TLeaf -> impossible() \
    | TNode { l = tl; v = tv; r = tr; _ }

  (* [weight t] reads and returns the weight of the tree [t]. *)

  let[@inline] weight t =
    match t with
    | TLeaf ->
        1
    | TNode { w; _ } ->
        w

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

  (* [siblings l r] checks that [l] and [r] could be siblings in a valid
     tree. *)

  let[@inline] like_weights wl wr =
    alpha * wr <= (100-alpha) * wl &&
    alpha * wl <= (100-alpha) * wr

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

  (* [create l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced and must have like weights. *)

  let[@inline] raw_create l v r =
    let w = weight l + weight r in
    TNode { l; v; r; w }

  let[@inline] create l v r =
    if debug then assert (siblings l r);
    raw_create l v r

  let join_weight_balanced l v r =
    if debug then assert (siblings l r);
    create l v r

  let[@inline] singleton x =
    (* This is equivalent to [create TLeaf x TLeaf]. *)
    TNode { l = TLeaf; v = x; r = TLeaf; w = 2 }

  let[@inline] doubleton x y =
    if debug then assert (E.compare x y < 0);
    TNode { l = TLeaf; v = x; r = singleton y; w = 3 }

  let[@inline] tripleton x y z =
    if debug then assert (E.compare x y < 0);
    if debug then assert (E.compare y z < 0);
    TNode { l = singleton x; v = y; r = singleton z; w = 4 }

  (* [is_singleton t] is equivalent to [weight t = 2]. *)

  (* Instead of testing whether the weight is 2, we could test whether
     both children are [TLeaf]. This makes essentially no difference. *)

  let[@inline] is_singleton t =
    match t with
    | TLeaf ->
        false
    | TNode { w; _ } ->
        w = 2

  (* [seems_smaller t1 t2] is equivalent to [weight t1 <= weight t2]. *)

  let[@inline] seems_smaller t1 t2 =
    match t1, t2 with
    | TLeaf, _ ->
        true
    | _, TLeaf ->
        false
    | TNode { w = w1; _ }, TNode { w = w2; _ } ->
        w1 < w2

  (* A left rotation. *)

  (* [rotate_left l v r] requires [l < v < r]. It applies a left rotation to
     the node [TNode { l; v; r }], without actually allocating this node. *)

  (* The subtree [r] must be nonempty: it is decomposed into [rl < rv < rr].
     Then, the new tree [create (create l v rl) rv rr] is constructed. For
     this new tree to be well-formed, [l] and [rl] must have like weights, and
     the combined weights of [l] and [rl] must be like the weight of [rr]. *)

  let[@inline] raw_rotate_left l v r =
    DESTRUCT(r, rl, rv, rr) ->
    raw_create (create l v rl) rv rr

  let[@inline] rotate_left l v r =
    DESTRUCT(r, rl, rv, rr) ->
    (* TODO once tested,remove these redundant assertions *)
    if verbose then printf "rotate_left: weight(l) = %d, weight(rl) = %d\n%!" (weight l) (weight rl);
    if debug then assert (siblings l rl);
    if verbose then printf "rotate_left: they are siblings (OK)\n%!";
    if verbose then printf "rotate_left: weight(rr) = %d\n%!" (weight rr);
    if verbose && rr = TLeaf then printf "rotate_left: rr is empty\n%!";
    if debug then assert (like_weights (weight l + weight rl) (weight rr));
    create (create l v rl) rv rr

  (* A right rotation. *)

  let[@inline] raw_rotate_right l v r =
    DESTRUCT(l, ll, lv, lr) ->
    raw_create ll lv (create lr v r)

  let[@inline] rotate_right l v r =
    DESTRUCT(l, ll, lv, lr) ->
    (* TODO once tested,remove these redundant assertions *)
    if debug then assert (siblings lr r);
    if debug then assert (like_weights (weight ll) (weight lr + weight r));
    create ll lv (create lr v r)

  (* [join_right] corresponds to [joinRightWB] in BFS, Figure 8. *)

  (* [join_right l v r] ... TODO In this recursive
     function, the parameter [r] is invariant: the right branch of the tree
     [l] is followed until a node with like weight to [r] is reached.*)

  (* TODO keep [wr] at hand; avoiding repeated weight computations *)

  let[@inline] balance_right_heavy l v r =
    if siblings l r then
      create l v r
    else
      DESTRUCT(r, rl, rv, rr) ->
      (* TODO can this complicated condition be simplified? *)
      if siblings l rl && like_weights (weight l + weight rl) (weight rr) then
        rotate_left l v r
      else
        rotate_left l v (raw_rotate_right rl rv rr)

  let rec join_right l v r =
    (* [weight r <= weight l] does NOT necessarily hold here. *)
    if siblings l r then
      create l v r
    else
      DESTRUCT(l, ll, lv, lr) ->
      balance_right_heavy ll lv (join_right lr v r)

  let rec join_left l v r =
    (* [weight l <= weight r] does NOT necessarily hold here. *)
    if verbose then printf "join_left: weight(l) = %d, weight(r) = %d\n%!" (weight l) (weight r);
    if siblings l r then begin
      if verbose then printf "join_left: like weights, happy\n%!";
      create l v r
    end
    else begin
      if verbose then printf "join_left: unlike weights\n%!";
      DESTRUCT(r, rl, rv, rr) ->
      let t1 = join_left l v rl in
      if debug then assert (weight t1 = weight l + weight rl); (* TODO *)
      if verbose then printf "join_left: back from call, weight(t1) = %d, weight(rr) = %d\n%!" (weight t1) (weight rr);
      if siblings t1 rr then begin
        if verbose then printf "join_left: like weights, happy\n%!";
        create t1 rv rr
      end
      else begin
        if verbose then printf "join_left: unlike weights\n%!";
        DESTRUCT(t1, l1, v1, r1) ->
        if verbose then printf "join_left: weight(l1) = %d, weight(r1) = %d\n%!" (weight l1) (weight r1);
        if siblings r1 rr && like_weights (weight l1) (weight r1 + weight rr) then begin
          if verbose then printf "join_left: a right rotation suffices\n%!";
          rotate_right t1 rv rr
        end
        else begin
          if verbose then printf "join_left: double rotation is necessary\n%!";
          rotate_right (raw_rotate_left l1 v1 r1) rv rr
        end
      end
    end

  (* [join l v r] requires [l < v < r]. It makes no assumptions about
     the weights of the subtrees [l] and [r]. *)

  (* TODO may wish to specialize join for the cases where [l] or [r]
     is empty; this is [add_min_element] and [add_max_element] *)

  let join l v r =
    let wl = weight l
    and wr = weight r in
    if like_weights wl wr then
      create l v r
    else if wr <= wl then
      join_right l v r (* TODO specialize [join_right] for the case where the siblings test will fail *)
    else
      join_left l v r  (* TODO same *)

  let join_neighbors =
    join

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
