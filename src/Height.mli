open Signatures

module Make (E : sig type t end) : BST with type key = E.t
