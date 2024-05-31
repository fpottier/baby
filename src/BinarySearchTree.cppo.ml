(* BFS = Blelloch, Ferizovic and Sun (2016). *)

open Signatures
open Profile

module[@inline] Make
(E : OrderedType)
(T : BST with type key = E.t)
= struct
open T

#define VIEW(t)       (view t)
#define LEAF          Leaf
#define NODE(x, y, z) Node (x, y, z)

#include "Basics.frag.ml"
#include "MinMax.frag.ml"
#include "Readers.frag.ml"
#include "Split.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Union.frag.ml"

end
