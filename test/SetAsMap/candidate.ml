(* This module implements a map (whose keys and values are integers)
   as a set of pairs. It is currently very incomplete. *)

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

let add k v m =
  if has_binding k v m then
    m
  else
    Set.add (k, v) (remove k m)

let cardinal =
  Set.cardinal

let is_empty =
  Set.is_empty
