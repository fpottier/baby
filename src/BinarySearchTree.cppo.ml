(* BFS = Blelloch, Ferizovic and Sun (2016). *)

open Signatures

module[@inline] Make
(E : OrderedType)
(T : BST with type key = E.t)
= struct
open T

#define VIEW(t)       (view t)
#define LEAF          Leaf
#define NODE(x, y, z) Node (x, y, z)

#include "MinMax.frag.ml"
#include "Readers.frag.ml"
#include "Split.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Union.frag.ml"

(* The set API. *)

type elt = key
type set = tree
type t = set

let empty =
  leaf

end
