(******************************************************************************)
(*                                                                            *)
(*                                    Baby                                    *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Signatures

#scope

#include "ConcreteView.macros"

module Set = struct
  module type OrderedType = OrderedType
  module type S = SET
  module[@inline] Make (E : OrderedType) = struct
    include WeightSet
    #include "UpperLayer.frag.ml"
  end
end

#endscope

#scope

#define MAP_VARIANT
#include "ConcreteView.macros"

module Map = struct
  module type OrderedType = OrderedType
  module type S = MAP
  module[@inline] Make (E : OrderedType) = struct
    include WeightMap
    #include "UpperLayer.frag.ml"
  end
end

#endscope

module[@inline] Make (E : OrderedType) = struct

  module Set = Set.Make(E)
  module Map = Map.Make(E)

  (* Because we have access to the concrete view of sets and maps, and because
     we know that sets and maps use the same balancing scheme, we can copy the
     balancing information (that is, the field [w]). We save the cost of
     recomputing this information at each node. *)

  (* Because we use concrete data constructors (as opposed to smart
     constructor functions), we can annotate these recursive functions with
     [@tail_mod_cons], which presumably makes them faster. *)

  let[@tail_mod_cons] rec domain m =
    match m with
    | WeightMap.TLeaf ->
        WeightSet.TLeaf
    | WeightMap.TNode { l; k; d; r; w } ->
        ignore d;
        let l = domain l in
        let v = k in
        WeightSet.TNode { l; v; r = domain r; w }

  let[@tail_mod_cons] rec lift f s =
    match s with
    | WeightSet.TLeaf ->
        WeightMap.TLeaf
    | WeightSet.TNode { l; v; r; w } ->
        (* Enforce left-to-right evaluation order. *)
        let l = lift f l in
        let k = v in
        let d = f k in
        WeightMap.TNode { l; k; d; r = lift f r; w }

end
