open Signatures

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
