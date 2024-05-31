(* A reference implementation of hash sets. *)

open Bbst.Signatures

module Make (E : OrderedType) = struct

  include Set.Make(E)

  let xor s1 s2 =
    union (diff s1 s2) (diff s2 s1)

end
