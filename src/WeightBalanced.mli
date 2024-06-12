open Signatures

module Make (E : OrderedType) : SET with type elt = E.t
