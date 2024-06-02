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

  val add_min_element : key -> tree -> tree

  (* TODO not part of the minimal interface: *)
  val singleton : key -> tree
  val doubleton : key -> key -> tree
  val tripleton : key -> key -> key -> tree
  val is_singleton : tree -> bool
  val seems_smaller : tree -> tree -> bool
  val siblings : tree -> tree -> bool
  val check : tree -> unit
end

module type SET = sig
  type elt
  type set
  type t = set
  val check : set -> unit
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
  val compare : set -> set -> int
  val elements : set -> elt list
  val to_seq : set -> elt Seq.t
  val of_list : elt list -> set
  val of_array : elt array -> set
  val of_sorted_unique_array : elt array -> set

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
