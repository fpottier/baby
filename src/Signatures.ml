module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type BST = sig
  type key
  type tree
  type view =
    | Leaf
    | Node of tree * key * tree
  val view : tree -> view
  val make : view -> tree
end
