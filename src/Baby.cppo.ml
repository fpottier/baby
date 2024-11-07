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

include Signatures

(* -------------------------------------------------------------------------- *)

(* The functor [Custom] constructs balanced binary search trees
   based on a user-supplied balancing scheme. *)

module[@inline] Custom
(BaseSet : BASE_SET)
(BaseMap : BASE_MAP)
= struct

  module Set = struct
    module type OrderedType = OrderedType
    module type S = SET
    module[@inline] Make (E : OrderedType) = struct
      include BaseSet
      #scope
      #include "AbstractView.macros"
      #include "UpperLayer.frag.ml"
      #endscope
    end
  end

  module Map = struct
    module type OrderedType = OrderedType
    module type S = MAP
    module[@inline] Make (E : OrderedType) = struct
      include BaseMap
      #scope
      #define MAP_VARIANT
      #include "AbstractView.macros"
      #include "UpperLayer.frag.ml"
      #endscope
    end
  end

  module[@inline] Make (E : OrderedType) = struct

    module Set = Set.Make(E)
    module Map = Map.Make(E)

    let rec domain m =
      match BaseMap.view m with
      | BaseMap.Leaf ->
          BaseSet.leaf
      | BaseMap.Node (l, v, r) ->
          let (k, _) = v in
          BaseSet.join_siblings (domain l) k (domain r)

    let rec lift f s =
      match BaseSet.view s with
      | BaseSet.Leaf ->
          BaseMap.leaf
      | BaseSet.Node (l, k, r) ->
          (* Enforce left-to-right evaluation order. *)
          let l = lift f l in
          let v = (k, f k) in
          let r = lift f r in
          BaseMap.join_siblings l v r

    end

end

(* -------------------------------------------------------------------------- *)

(* The module [Baby.H] provides ready-made height-balanced binary
   search trees. *)

(* Unfortunately, the OCaml compiler is pretty bad at optimization. In my
   experience, although it does usually inline functions when requested, it
   does not subsequently perform the simplifications that one might naturally
   expect. In particular, it does not simplify match-of-match, and cannot even
   simplify match-of-constructor. *)

(* For this reason, instead of applying the functor [Custom] (above), we
   inline it, using a preprocessor hack. Thus, we avoid the overhead of going
   through a [view] function; instead, we have a [VIEW] macro. *)

module H = H

(* -------------------------------------------------------------------------- *)

(* The module [Baby.W] provides ready-made weight-balanced binary
   search trees. *)

module W = W

(* -------------------------------------------------------------------------- *)

(* The following modules must be exported, because they are (or may be) used
   in the benchmarks. Because they are somewhat unlikely to be useful to an
   end user, and because they may change in th future, their existence is not
   advertised. *)

module HeightSet = HeightSet
module WeightSet = WeightSet

module HeightMap = HeightMap
module WeightMap = WeightMap
