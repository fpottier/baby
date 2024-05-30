(* BFS = Blelloch, Ferizovic and Sun (2016). *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type BST = sig
  type key
  type tree
  type view =
    | Leaf
    | Node of tree * key * tree
  val view : tree -> view
  val make : view -> tree
end

(* The following code is taken (with adaptations) from OCaml's Set library. *)

module[@inline] OCamlSet
(E : sig type t end)
= struct

  type key = E.t

  (* Trees are height-balanced. Each node stores its left child, key, right
     child, and height. The code maintains the invariant that the heights of
     the two children differ by at most 2. *)

  type tree =
    | TLeaf
    | TNode of { l : tree; v : key; r : tree; h : int }

  let[@inline] height t =
    match t with
    | TLeaf ->
        0
    | TNode { h; _ } ->
        h

  let[@inline] max (x : int) (y : int) =
    if x <= y then y else x

  let(* not inlined *) impossible () =
    assert false

  let[@inline] destruct t =
    match t with
    | TLeaf ->
        impossible()
    | TNode { l; v; r; _ } ->
        l, v, r

  (* [create l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced, and the difference in their heights must be at most 2. *)

  let[@inline] create l v r =
    let h = max (height l) (height r) + 1 in
    TNode { l; v; r; h }

  let[@inline] singleton x =
    (* This is equivalent to [create TLeaf x TLeaf]. *)
    TNode { l = TLeaf; v = x; r = TLeaf; h = 1 }

  (* [bal l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced, and the difference in their heights must be at most 3. If
     necessary, one step of rebalancing is performed. *)

  let bal l v r =
    let hl = height l
    and hr = height r in
    if hl > hr + 2 then begin
      let ll, lv, lr = destruct l in
      if height ll >= height lr then
        create ll lv (create lr v r)
      else
        let lrl, lrv, lrr = destruct lr in
        create (create ll lv lrl) lrv (create lrr v r)
    end
    else if hr > hl + 2 then begin
      let rl, rv, rr = destruct r in
      if height rr >= height rl then
        create (create l v rl) rv rr
      else
        let rll, rlv, rlr = destruct rl in
        create (create l v rll) rlv (create rlr rv rr)
    end
    else
      (* This is equivalent to [create l v r]. *)
      let h = max hl hr + 1 in
      TNode { l; v; r; h }

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
    | TNode { l = ll; v = lv; r = lr; h = lh },
      TNode { l = rl; v = rv; r = rr; h = rh } ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
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

  let[@inline] make v =
    match v with
    | Leaf ->
        TLeaf
    | Node (l, v, r) ->
        join l v r

end

module[@inline] Make
(E : OrderedType)
(T : BST with type key = E.t)
= struct
open T

(* [node] is known as [join] in BFS. *)

(* BFS write: "the cost of [join] must be proportional to the difference in
   ranks of two trees, and the rank of the result of a join must be at most
   one more than the maximum rank of the two arguments". *)

let leaf : tree =
  make Leaf

let[@inline] node (l : tree) (k : key) (r : tree) : tree =
  make (Node (l, k, r))

let[@inline] singleton (k : key) =
  node leaf k leaf

let rec split (k : key) (t : tree) : tree * bool * tree =
  match view t with
  | Leaf ->
      leaf, false, leaf
  | Node (l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        l, true, r
      else if c < 0 then
        let ll, b, lr = split k l in
        ll, b, node lr m r
      else
        let rl, b, rr = split k r in
        node l m rl, b, rr

let rec split_last (l : tree) (k : key) (r : tree) : tree * key =
  match view r with
  | Leaf ->
      l, k
  | Node (l', k', r') ->
      let r, m = split_last l' k' r' in
      node l k r, m

(* [join2] is known as [concat] in OCaml's Set library. *)
let join2 (l : tree) (r : tree) : tree =
  match view l with
  | Leaf ->
      r
  | Node (ll, m, lr) ->
      let l', k = split_last ll m lr in
      node l' k r

(* This is insertion in the style of BFS. *)
let _simple_add (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  node l k r

(* This is a less elegant but more efficient version of insertion. *)
(* In this direct implementation of [add],
   OCaml's Set library uses [bal], but we must use [node],
   which is slightly less efficient. *)
let rec add (x : key) (t : tree) : tree =
  match view t with
  | Leaf ->
      singleton x
  | Node (l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let ll = add x l in
        if l == ll then t else node ll v r
      else
        let rr = add x r in
        if r == rr then t else node l v rr

let remove (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join2 l r

let rec union (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _ ->
      t2
  | _, Leaf ->
      t1
  | Node _, Node (l2, k2, r2) ->
      let l1, _, r1 = split k2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      node l k2 r

let rec inter (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _
  | _, Leaf ->
      leaf
  | Node _, Node (l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = inter l1 l2
      and r = inter r1 r2 in
      if b then node l k2 r else join2 l r

let rec diff (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _ ->
      leaf
  | _, Leaf ->
      t1
  | Node _, Node (l2, k2, r2) ->
      let l1, _, r1 = split k2 t1 in
      let l = diff l1 l2
      and r = diff r1 r2 in
      join2 l r

let rec xor (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _ ->
      t2
  | _, Leaf ->
      t1
  | Node _, Node (l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = xor l1 l2
      and r = xor r1 r2 in
      if b then join2 l r else node l k2 r

(* The set API. *)

type elt = key
type set = tree
type t = set

let empty =
  leaf

end

module[@inline] M (E : OrderedType) =
  Make(E)(OCamlSet(E))

(* TODO
 + implement more functions from BFS
 + benchmark against OCaml Set using the same data representation
 + perform manual CSE in [bal]
 + could optimize [split] for the case where the Boolean result is not needed
 + explore parallel computation (perhaps even with futures?)
*)

module Flat (E : OrderedType) = struct

  type key = E.t

  (* Trees are height-balanced. Each node stores its left child, key, right
     child, and height. The code maintains the invariant that the heights of
     the two children differ by at most 2. *)

  type tree =
    | TLeaf
    | TNode of { l : tree; v : key; r : tree; h : int }

  let[@inline] height t =
    match t with
    | TLeaf ->
        0
    | TNode { h; _ } ->
        h

  let[@inline] max (x : int) (y : int) =
    if x <= y then y else x

  let(* not inlined *) impossible () =
    assert false

  (* [create l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced, and the difference in their heights must be at most 2. *)

  let[@inline] create l v r =
    let h = max (height l) (height r) + 1 in
    TNode { l; v; r; h }

  let[@inline] singleton x =
    (* This is equivalent to [create TLeaf x TLeaf]. *)
    TNode { l = TLeaf; v = x; r = TLeaf; h = 1 }

  (* [bal l v r] requires [l < v < r]. It constructs a node with left child
     [l], value [v], and right child [r]. The subtrees [l] and [r] must be
     balanced, and the difference in their heights must be at most 3. If
     necessary, one step of rebalancing is performed. *)

  let bal l v r =
    let hl = height l
    and hr = height r in
    if hl > hr + 2 then begin
      match l with
      | TLeaf -> impossible()
      | TNode { l = ll; v = lv; r = lr; _ } ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else
        match lr with
        | TLeaf -> impossible()
        | TNode { l = lrl; v = lrv; r = lrr; _ } ->
        create (create ll lv lrl) lrv (create lrr v r)
    end
    else if hr > hl + 2 then begin
      match r with
      | TLeaf -> impossible()
      | TNode { l = rl; v = rv; r = rr; _ } ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else
        match rl with
        | TLeaf -> impossible()
        | TNode { l = rll; v = rlv; r = rlr; _ } ->
        create (create l v rll) rlv (create rlr rv rr)
    end
    else
      (* This is equivalent to [create l v r]. *)
      let h = max hl hr + 1 in
      TNode { l; v; r; h }

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
    | TNode { l = ll; v = lv; r = lr; h = lh },
      TNode { l = rl; v = rv; r = rr; h = rh } ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
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

  let[@inline] make v =
    match v with
    | Leaf ->
        TLeaf
    | Node (l, v, r) ->
        join l v r

(* [node] is known as [join] in BFS. *)

(* BFS write: "the cost of [join] must be proportional to the difference in
   ranks of two trees, and the rank of the result of a join must be at most
   one more than the maximum rank of the two arguments". *)

let leaf : tree =
  make Leaf

let[@inline] node (l : tree) (k : key) (r : tree) : tree =
  make (Node (l, k, r))

(* TODO
let[@inline] singleton (k : key) =
  node leaf k leaf
 *)

let rec split (k : key) (t : tree) : tree * bool * tree =
  match t with
  | TLeaf ->
      leaf, false, leaf
  | TNode { l; v = m; r; _ } ->
      let c = E.compare k m in
      if c = 0 then
        l, true, r
      else if c < 0 then
        let ll, b, lr = split k l in
        ll, b, node lr m r
      else
        let rl, b, rr = split k r in
        node l m rl, b, rr

let rec split_last (l : tree) (k : key) (r : tree) : tree * key =
  match view r with
  | Leaf ->
      l, k
  | Node (l', k', r') ->
      let r, m = split_last l' k' r' in
      node l k r, m

(* [join2] is known as [concat] in OCaml's Set library. *)
let join2 (l : tree) (r : tree) : tree =
  match view l with
  | Leaf ->
      r
  | Node (ll, m, lr) ->
      let l', k = split_last ll m lr in
      node l' k r

(* This is insertion in the style of BFS. *)
let _simple_add (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  node l k r

(* This is a less elegant but more efficient version of insertion. *)
(* In this direct implementation of [add],
   we use OCaml's Set library uses [bal]
   instead of [node],
   because it is safe to use in this case, and more efficient. *)
let rec add (x : key) (t : tree) : tree =
  match t with
  | TLeaf ->
      singleton x
  | TNode { l; v; r; _ } ->
      let c = E.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let ll = add x l in
        if l == ll then t else bal ll v r
      else
        let rr = add x r in
        if r == rr then t else bal l v rr

let remove (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join2 l r

let rec union (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _ ->
      t2
  | _, Leaf ->
      t1
  | Node _, Node (l2, k2, r2) ->
      let l1, _, r1 = split k2 t1 in
      let l = union l1 l2
      and r = union r1 r2 in
      node l k2 r

let rec inter (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _
  | _, Leaf ->
      leaf
  | Node _, Node (l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = inter l1 l2
      and r = inter r1 r2 in
      if b then node l k2 r else join2 l r

let rec diff (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _ ->
      leaf
  | _, Leaf ->
      t1
  | Node _, Node (l2, k2, r2) ->
      let l1, _, r1 = split k2 t1 in
      let l = diff l1 l2
      and r = diff r1 r2 in
      join2 l r

let rec xor (t1 : tree) (t2 : tree) : tree =
  match view t1, view t2 with
  | Leaf, _ ->
      t2
  | _, Leaf ->
      t1
  | Node _, Node (l2, k2, r2) ->
      let l1, b, r1 = split k2 t1 in
      let l = xor l1 l2
      and r = xor r1 r2 in
      if b then join2 l r else node l k2 r

(* The set API. *)

type elt = key
type set = tree
type t = set

let empty =
  leaf

end
