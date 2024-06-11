module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

(**The signature [BST] describes the interface offer by the balancing code.
   The remaining operations on binary search tree are built on top of this
   interface, and are oblivious to the balancing criterion. *)
module type BST = sig

  (**Keys, or elements. *)
  type key

  (**Balanced binary search trees. *)
  type tree

  (**A view on a balanced binary search tree indicates whether this tree
     is a leaf or a node, and, if it is a node, gives access to its left
     child, its key, and its right child. A view does not give access to
     balancing information, such as the tree's height or weight. *)
  type view =
    | Leaf
    | Node of tree * key * tree

  (**[view] turns a tree into a view. *)
  val view : tree -> view

  (* In the reverse direction, one could imagine a conversion function
     [make : view -> tree]. In order to avoid a memory allocation, we
     replace this function with a constant [leaf] and a function [join]. *)

  (**[leaf] is the empty tree; a leaf. *)
  val leaf : tree

  (**[join l v r] expects a subtree [l], a key [v], and a subtree [r] such
     that [l < v < r] holds. It returns a new tree whose elements are the
     elements of [l], [v], and [r]. If needed, it performs rebalancing, so the
     key [v] is not necessarily found at the root of the new tree. *)
  val join : tree -> key -> tree -> tree
  (* Regarding computational complexity, BFS write: the cost of [join] must be
     proportional to the difference in ranks of two trees, and the rank of the
     result of a join must be at most one more than the maximum rank of the
     two arguments. *)

  (* The following functions are not part of the minimal interface proposed
     by BFS. Exposing these functions allow us to gain efficiency in various
     situations. *)

  (**[join_neighbors l v r] is analogous to [join l v r]. Like [join], it
     requires [l < v < r]. Furthermore, it assumes that the trees [l] and [r]
     have been obtained by taking two siblings in a well-formed tree and by
     adding or removing one element in one of them. *)
  val join_neighbors : tree -> key -> tree -> tree

  (**[join_weight_balanced l v r] is analogous to [join l v r]. Like [join],
     it requires [l < v < r]. Furthermore, it assumes that the weights of the
     trees [l] and [r] differ by at most one. *)
  val join_weight_balanced : tree -> key -> tree -> tree

  (**If the weight of a tree can be determined in constant time, then [weight
     t] returns the weight of the tree [t]. If the weight of a tree cannot be
     efficiently determined, then it is acceptable for [weight] to always
     return zero. The function [weight] is used to implement fast paths in
     subset and equality tests: it must be the case that [subset t1 t2]
     implies [weight t1 <= weight t2]. *)
  val weight : tree -> int

  (**[singleton x] constructs a tree whose sole element is [x]. *)
  val singleton : key -> tree

  (**[doubleton x y] requires [x < y]. It constructs a tree whose elements are
     [x] and [y]. *)
  val doubleton : key -> key -> tree

  (**[tripleton x y z] requires [x < y < z]. It constructs a tree whose
     elements are [x], [y], and [z]. *)
  val tripleton : key -> key -> key -> tree

  (**[seems_smaller t1 t2] indicates which of the trees [t1] and [t2] seems
     smaller, based on height or weight. This function is used as part of a
     heuristic choice, so no correctness obligation bears on it; its
     postcondition is [true]. *)
  val seems_smaller : tree -> tree -> bool

  (**[siblings t1 t2] determines whether the trees [t1] and [t2] can be
     siblings in a valid tree. It is used in debugging assertions only. *)
  val siblings : tree -> tree -> bool

  (**[check t] checks that the tree [t] is well-formed: that is, [t] is a
     balanced binary search tree. This function is used while testing only. *)
  val check : tree -> unit

end

module type SET = sig
  type elt
  type set
  type t = set
  val check : set -> unit
  val empty : set
  val is_empty : set -> bool
  val singleton : elt -> t
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
  val equal : set -> set -> bool
  val compare : set -> set -> int
  val split : elt -> set -> set * bool * set
  val elements : set -> elt list
  val to_seq : set -> elt Seq.t
  val of_list : elt list -> set
  val of_array : elt array -> set
  val of_sorted_unique_array : elt array -> set
  val to_array : set -> elt array
  val cardinal : set -> int
  val get : set -> int -> elt

  module Enum : sig

    type set = t
    (** The type [set] is a synonym for the type of sets. *)

    type enum
    (** The type of enumerations. An enumeration represents an increasing
        sequence of elements of type [elt]. *)

    type t = enum
    (** A synonym for the type [enum]. *)

    val empty : enum
    (** [empty] is the empty enumeration. It contains zero elements. *)

    val is_empty : enum -> bool
    (** [is_empty e] tests whether the enumeration [e] is empty. *)

    val enum : set -> enum
    (** [enum s] returns an enumeration of the set [s]. This enumeration
        contains all of the elements of the set [s], in increasing order. *)

    val enum_from : elt -> set -> enum
    (** [enum_from x s] returns an enumeration of the subset of [s]
        formed of just the elements that are no less than [x].
        It is equivalent to [from x (enum s)]. *)

    val head : enum -> elt
    (** [head e] returns the first element of the enumeration [e].
        The enumeration [e] must be nonempty. *)

    val tail : enum -> enum
    (** [tail e] returns the enumeration [e], deprived of its first element.
        The enumeration [e] must be nonempty. *)

    val head_opt : enum -> elt option
    (** If the enumeration [e] is nonempty, then [head_opt e] returns its
        first element. Otherwise, it returns [None]. *)

    val tail_opt : enum -> enum option
    (** If the enumeration [e] is nonempty, then [tail_opt e] returns the
        enumeration [e], deprived of its first element. Otherwise, it
        returns [None]. *)

    val from : elt -> enum -> enum
    (** [from x e] returns the enumeration obtained from the enumeration [e]
        by skipping the elements that lie below the threshold [x]. In other
        words, only the elements of [e] that lie at or above the threshold
        [x] are retained. *)

    val to_seq : enum -> elt Seq.t
    (** [to_seq] converts an enumeration into a (persistent) sequence. *)

    val elements : enum -> set
    (** [elements] converts an enumeration into a set. *)

  end (* Enum *)

end
