(* BFS = Blelloch, Ferizovic and Sun (2016). *)

open Signatures
open Profile

module[@inline] Make
(E : OrderedType)
(T : BST with type key = E.t)
= struct
include T

  #define VIEW(t)       (view t)
  #define LEAF          Leaf
  #define NODE(x, y, z) Node (x, y, z)

  #include "Cardinal.frag.ml"
  #include "Common.frag.ml"

end
