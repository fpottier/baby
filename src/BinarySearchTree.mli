open Signatures

module Make
(E : OrderedType)
(_ : BST with type key = E.t)
: sig
  type elt = E.t
  type set
  type t = set
  val empty : set
  val add : elt -> set -> set
  val remove : elt -> set -> set
  val union : set -> set -> set
  val inter : set -> set -> set
  val diff : set -> set -> set
  val xor : set -> set -> set
end

module Flat (E : OrderedType) : sig
  type elt = E.t
  type set
  type t = set
  val empty : set
  val add : elt -> set -> set
  val remove : elt -> set -> set
  val union : set -> set -> set
  val inter : set -> set -> set
  val diff : set -> set -> set
  val xor : set -> set -> set
end
