(* BFS = Blelloch, Ferizovic and Sun (2016). *)

open Signatures

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

#define VIEW(t)       (view t)
#define LEAF          Leaf
#define NODE(x, y, z) Node (x, y, z)

(* [mem] *)

#include "Readers.frag.ml"

(* [split] *)
(* [split2] *)
(* [split_last] *)
(* [join2] *)

#include "Split.frag.ml"

(* [add] *)

#include "Add.frag.ml"

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
