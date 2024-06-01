open Signatures
open Profile

(* Unfortunately, the OCaml compiler is pretty bad at optimization. In my
   experience, although it does usually inline functions when this is
   requested, it does not subsequently perform the simplifications that one
   might naturally expect. In particular, it does not simplify match-of-match,
   and cannot even simplify match-of-constructor. *)

module[@inline] Make (E : OrderedType) = struct

include Height.Make(E)

#define VIEW(t)       (t)
#define LEAF          TLeaf
#define NODE(x, y, z) TNode { l = x; v = y; r = z; _ }

#include "Basics.frag.ml"
#include "MinMax.frag.ml"
#include "Readers.frag.ml"
#include "Split.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Union.frag.ml"
#include "Conversions.frag.ml"

end
