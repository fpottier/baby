(* This module implements a map (whose keys and values are integers)
   as a set of pairs. *)

module Key = Int

module Val = Int

module Pair = struct
  type t = Key.t * Val.t
  let compare (k1, _) (k2, _) = Key.compare k1 k2
end

module Set =
  Baby.W.Set.Make(Pair)

let check =
  Set.check

let empty =
  Set.empty

let singleton k v =
  Set.singleton (k, v)

(* TODO need variants of [Set.mem] and [Set.find] and [Set.remove] that take
   the partial application [compare x] as an argument, instead of [x]
   itself. *)

let dummy = 0

let mem k m =
  Set.mem (k, dummy) m

let find k m =
  let _k, v = Set.find (k, dummy) m in
  v

let remove k m =
  Set.remove (k, dummy) m

(* [has_binding k v m] tests whether the map [m] contains a binding of key
   [k] to value [v]. Physical equality is used to compare values. *)
let has_binding k v m =
  match find k m with
  | exception Not_found -> false
  | v' -> v' == v

(* TODO need a variant of [Set.add] which always replaces the old value
   with the new value, and preserves physical equality if the two values
   are physically equal *)

let add k v m =
  if has_binding k v m then
    m
  else
    Set.add (k, v) (remove k m)

(* TODO need a variant of [Set.union] where the new element is computed
   as a function of the previous two elements? but the function must
   promise to return an element that is equivalent to the two elements
   that it receives (dynamic check). *)

let cardinal =
  Set.cardinal

let is_empty =
  Set.is_empty

(* TODO need a variant of [Set.equal] that is parameterized with an
   equality function on values *)

(* TODO need a variant of [Set.compare] that is parameterized with an
   ordering function on values *)

(* TODO to implement enumerations on maps, need a variant of [Set.Enum.from]
   that takes [compare x] as an argument. This may be related to [find_first]. *)
