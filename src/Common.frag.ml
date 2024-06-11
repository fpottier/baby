(* -------------------------------------------------------------------------- *)

(* Types. *)

type elt = key
type set = tree
type t = set

(* -------------------------------------------------------------------------- *)

(* Macros. *)

(* [EMPTY(t)] determines whether the tree [t] is empty, that is, a leaf. *)

#define EMPTY(t)        (match VIEW(t) with LEAF -> true | _ -> false)

(* [BOTH_EMPTY(l,r)] determines whether the trees [l] and [r] are both empty. *)

#define BOTH_EMPTY(l,r) (EMPTY(l) && EMPTY(r))

(* -------------------------------------------------------------------------- *)

(* Operations. *)

#include "Empty.frag.ml"
#include "MinMax.frag.ml"
#include "Mem.frag.ml"
#include "Find.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Split.frag.ml"
#include "Enum.frag.ml"
#include "Compare.frag.ml"
#include "Equal.frag.ml"
#include "Union.frag.ml"
#include "Inter.frag.ml"
#include "Diff.frag.ml"
#include "Xor.frag.ml"
#include "Disjoint.frag.ml"
#include "Subset.frag.ml"
#include "Conversions.frag.ml"
#include "Map.frag.ml"
