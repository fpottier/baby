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

  (* TODO not part of the minimal interface: *)
  val is_singleton : tree -> bool
  val seems_smaller : tree -> tree -> bool
end
