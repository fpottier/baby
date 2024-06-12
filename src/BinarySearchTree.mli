open Signatures

module Make
(E : OrderedType)
(_ : BST with type key = E.t)
: SET with type elt = E.t
