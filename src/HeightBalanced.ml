open Signatures

module[@inline] Make (E : OrderedType) = struct

  include Height.Make(E)

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
