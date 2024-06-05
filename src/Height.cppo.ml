open Signatures
open Profile

(* The following code taken from OCaml's Set library, and slightly adapted. *)

let[@inline] max (x : int) (y : int) =
  if x <= y then y else x

(* This function is not inlined, so as to reduce code size and produce
   more readable assembly code. *)
let impossible () =
  assert false

(* Although this functor requires an ordered type, the ordering function
   [E.compare] is used in assertions only. *)
module[@inline] Make (E : OrderedType) = struct

  type key = E.t

  (* Trees are height-balanced. Each node stores its left child, key, right
     child, and height. The code maintains the invariant that the heights of
     the two children differ by at most 2. *)

  type tree =
    | TLeaf
    | TNode of { l : tree; v : key; r : tree; h : int }

  (* This macro destructs a tree [t] that is known not to be a leaf. *)

  #define DESTRUCT(t,tl,tv,tr) \
    match t with \
    | TLeaf -> impossible() \
    | TNode { l = tl; v = tv; r = tr; _ }

  (* [height t] reads and returns the height of the tree [t]. *)

  let[@inline] height t =
    match t with
    | TLeaf ->
        0
    | TNode { h; _ } ->
        h

  (* The weight of a tree cannot be determined in constant time. *)

  let[@inline] weight _t =
    0

  (* [siblings l r] checks that [l] and [r] could be siblings in a valid
     tree. [neighbors l r] checks that [l] and [r] could be neighbors,
     that is, siblings where one tree has been disturbed by removing or
     adding one element. *)

  let siblings l r =
    abs (height l - height r) <= 2

  let neighbors l r =
    abs (height l - height r) <= 3

  (* A well-formedness check. *)

  let rec check t =
    match t with
    | TLeaf ->
        ()
    | TNode { l; r; h; _ } ->
        check l;
        check r;
        assert (h = max (height l) (height r) + 1);
        assert (siblings l r)

  (* [create l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced, and the difference in their heights must be at most 2. *)

  let[@inline] create l v r =
    if debug then assert (siblings l r);
    let h = max (height l) (height r) + 1 in
    TNode { l; v; r; h }

  let join_weight_balanced =
    create

  let[@inline] singleton x =
    (* This is equivalent to [create TLeaf x TLeaf]. *)
    TNode { l = TLeaf; v = x; r = TLeaf; h = 1 }

  let[@inline] doubleton x y =
    if debug then assert (E.compare x y < 0);
    TNode { l = TLeaf; v = x; r = singleton y; h = 2 }

  let[@inline] tripleton x y z =
    if debug then assert (E.compare x y < 0);
    if debug then assert (E.compare y z < 0);
    TNode { l = singleton x; v = y; r = singleton z; h = 2 }

  (* [is_singleton t] is equivalent to [height t = 1]. *)

  (* Instead of testing whether the height is 1, we could test whether
     both children are [TLeaf]. This makes essentially no difference. *)

  let[@inline] is_singleton t =
    match t with
    | TLeaf ->
        false
    | TNode { h; _ } ->
        h = 1

  (* [seems_smaller t1 t2] is equivalent to [height t1 <= height t2]. *)

  let[@inline] seems_smaller t1 t2 =
    match t1, t2 with
    | TLeaf, _ ->
        true
    | _, TLeaf ->
        false
    | TNode { h = h1; _ }, TNode { h = h2; _ } ->
        h1 < h2

  (* [bal l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced, and the difference in their heights must be at most 3. If
     necessary, one step of rebalancing is performed. *)

  (* Because [create] calls [height], this code involves several redundant
     computations of the height of a subtree. However, modifying the code to
     avoid this redundancy makes it much less readable and makes no measurable
     difference in the run time. *)

  let bal l v r =
    if debug then assert (neighbors l r);
    let hl = height l
    and hr = height r in
    if hl > hr + 2 then begin
      DESTRUCT(l, ll, lv, lr) ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else
        DESTRUCT(lr, lrl, lrv, lrr) ->
        create (create ll lv lrl) lrv (create lrr v r)
    end
    else if hr > hl + 2 then begin
      DESTRUCT(r, rl, rv, rr) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else
        DESTRUCT(rl, rll, rlv, rlr) ->
        create (create l v rll) rlv (create rlr rv rr)
    end
    else
      (* This is equivalent to [create l v r]. *)
      let h = max hl hr + 1 in
      TNode { l; v; r; h }

  let join_neighbors =
    bal

  (* [add_min_element x t] requires [x < t]. *)

  let rec add_min_element x t =
    match t with
    | TLeaf ->
        singleton x
    | TNode { l; v; r; _ } ->
        bal (add_min_element x l) v r

  (* [add_max_element x t] requires [t < x]. *)

  let rec add_max_element x t =
    match t with
    | TLeaf ->
        singleton x
    | TNode { l; v; r; _ } ->
        bal l v (add_max_element x r)

  (* [join l v r] requires [l < v < r]. It makes no assumptions about
     the heights of the subtrees [l] and [r]. *)

  let rec join l v r =
    match l, r with
    | TLeaf, _ ->
        add_min_element v r
    | _, TLeaf ->
        add_max_element v l
    | TNode { l = ll; v = lv; r = lr; h = hl },
      TNode { l = rl; v = rv; r = rr; h = hr } ->
        if hl > hr + 2 then bal ll lv (join lr v r) else
        if hr > hl + 2 then bal (join l v rl) rv rr else
        create l v r

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
