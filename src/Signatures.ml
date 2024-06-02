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

  (* TODO val make : view -> tree *)
  val leaf : tree
  val join : tree -> key -> tree -> tree
  (* BFS write: "the cost of [join] must be proportional to the difference in
     ranks of two trees, and the rank of the result of a join must be at most
     one more than the maximum rank of the two arguments". *)
  val join_neighbors : tree -> key -> tree -> tree
    (* assumes that the two trees [l] and [r] were siblings in a well-formed tree
       and that one of them has been disturbed by adding or removing one element. *)

  val join_weight_balanced : tree -> key -> tree -> tree
    (* assumes that the weights of the two trees differ by at most one *)

  (* TODO not part of the minimal interface: *)
  val singleton : key -> tree
  val doubleton : key -> key -> tree
  val tripleton : key -> key -> key -> tree
  val is_singleton : tree -> bool
  val seems_smaller : tree -> tree -> bool
  val siblings : tree -> tree -> bool
end

module type SET = sig
  type elt
  type set
  type t = set
  val empty : set
  val is_empty : set -> bool
  val min_elt : set -> elt
  val min_elt_opt : set -> elt option
  val max_elt : set -> elt
  val max_elt_opt : set -> elt option
  val mem : elt -> set -> bool
  val add : elt -> set -> set
  val remove : elt -> set -> set
  val remove_min_elt : set -> set
  val remove_max_elt : set -> set
  val union : set -> set -> set
  val inter : set -> set -> set
  val disjoint : set -> set -> bool
  val diff : set -> set -> set
  val subset : set -> set -> bool
  val xor : set -> set -> set
  val elements : set -> elt list
  val of_list : elt list -> set
  val of_sorted_unique_array : elt array -> set
  val of_array : elt array -> set
end
