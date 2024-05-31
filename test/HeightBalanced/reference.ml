(* A reference implementation of hash sets. *)

open Bbst.Signatures

module Make (E : OrderedType) = struct

  include Set.Make(E)

end
