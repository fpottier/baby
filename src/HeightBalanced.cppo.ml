open Signatures

(* Unfortunately, the OCaml compiler is pretty bad at optimization. In my
   experience, although it does usually inline functions when this is
   requested, it does not subsequently perform the simplifications that one
   might naturally expect. In particular, it does not simplify match-of-match,
   and cannot even simplify match-of-constructor. *)

module[@inline] Make (E : OrderedType) = struct

include Height.Make(E)

(* [node] is known as [join] in BFS. *)

(* BFS write: "the cost of [join] must be proportional to the difference in
   ranks of two trees, and the rank of the result of a join must be at most
   one more than the maximum rank of the two arguments". *)

(* [leaf] is equivalent to [make Leaf]. *)

let leaf : tree =
  TLeaf

(* [node l k r] is equivalent to [make (Node (l, k, r))]. *)

let node =
  join

#define VIEW(t)       (t)
#define LEAF          TLeaf
#define NODE(x, y, z) TNode { l = x; v = y; r = z; _ }

(* [split] *)
(* [split2] *)
(* [split_last] *)
(* [join2] *)

#include "Split.frag.ml"

(* [add] *)

(* This implementation of [add] is taken from OCaml's Set library. It
   uses [bal] instead of [node] because [bal] is safe to use in this
   case and more efficient. *)

let rec add (x : key) (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      singleton x
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let ll = add x l in
        if l == ll then t else bal ll v r
      else
        let rr = add x r in
        if r == rr then t else bal l v rr

(* [remove] *)

#include "Remove.frag.ml"

(* [union] *)
(* [inter] *)
(* [diff] *)
(* [xor] *)

#include "Union.frag.ml"

(* The set API. *)

type elt = key
type set = tree
type t = set

let empty =
  leaf

end
