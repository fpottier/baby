(* A reference implementation of hash sets. *)

open Bbst.Signatures

module Make (E : OrderedType) = struct

  include Set.Make(E)

  let remove_min_elt s =
    let x = min_elt s in
    remove x s

  let remove_max_elt s =
    let x = max_elt s in
    remove x s

  let xor s1 s2 =
    union (diff s1 s2) (diff s2 s1)

end
