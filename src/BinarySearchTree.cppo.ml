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
