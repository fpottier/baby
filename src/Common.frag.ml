(* [EMPTY(t)] determines whether the tree [t] is empty, that is, a leaf. *)

#define EMPTY(t)        (match VIEW(t) with LEAF -> true | _ -> false)

(* [BOTH_EMPTY(l,r)] determines whether the trees [l] and [r] are both empty. *)

#define BOTH_EMPTY(l,r) (EMPTY(l) && EMPTY(r))

#include "Basics.frag.ml"
#include "MinMax.frag.ml"
#include "Add.frag.ml"
#include "Remove.frag.ml"
#include "Split.frag.ml"
#include "Readers.frag.ml"
#include "Union.frag.ml"
#include "Conversions.frag.ml"
