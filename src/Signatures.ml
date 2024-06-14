(**The signature [OrderedType] describes a type equipped
   with a total ordering function. *)
module type OrderedType = sig

  (**The type of the set elements. *)
  type t

  (** A total ordering function over values of type [t].
      [compare x1 x2] must be zero if [x1] and [x2] are equal.
      It must be strictly negative if [x1] is smaller than [x2].
      It must be strictly positive if [x1] is greater than [x2]. *)
  val compare : t -> t -> int

end

(**The signature [BST] describes the interface that must be offered by
   the balancing code to the rest of the balanced binary search tree
   library. Most operations on binary search tree are built on top of
   this interface, and are oblivious to the balancing criterion. *)
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

  (**[cardinal t] returns the number of elements in the tree. Depending on the
     internal representation of trees, the function [cardinal] may have time
     complexity O(1) or O(n). This is indicated by [constant_time_cardinal]. *)
  val cardinal : tree -> int

  (**[constant_time_cardinal] indicates whether [cardinal] constant time
     complexity. *)
  val constant_time_cardinal : bool

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

(**The signature [SET] describes an abstract type of sets. *)
module type SET = sig

  (**The type of elements. *)
  type elt

  (**The abstract type of sets. *)
  type set

  (**A synonym for the type [set]. *)
  type t = set

  (** {1:construct Constructing sets} *)

  (**[empty] is the empty set. *)
  val empty : set

  (**[singleton x] returns a set whose sole element is [x]. *)
  val singleton : elt -> t

  (**[add x s] returns a set that contains all elements of the set
     [s], plus [x].
     Thus, it is logically equal to [union (singleton x) s].

     If the result is logically equal to [s], then
     the result is physically equal to [s].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val add : elt -> set -> set

  (**[remove x s] returns a set that contains all elements of the set
     [s], except [x].
     It is equivalent to [diff s (singleton x)].

     If the result is logically equal to [s], then
     the result is physically equal to [s].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val remove : elt -> set -> set

  (**If the set [s] is nonempty, then [remove_min_elt s] returns the
     set [s], deprived of its minimum element. Otherwise, it raises
     [Not_found].

     It is equivalent to [remove (min_elt s) s].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val remove_min_elt : set -> set

  (**If the set [s] is nonempty, then [remove_max_elt s] returns the
     set [s], deprived of its maximum element. Otherwise, it raises
     [Not_found].

     It is equivalent to [remove (max_elt s) s].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val remove_max_elt : set -> set

  (**[union s1 s2] returns the union of the sets [s1] and [s2], that
     is, a set that contains all elements of the set [s1] and all
     elements of the set [s2].

     The weight-balanced-tree implementation ({!Bistro.W}) offers
     the following guarantee:
     if the result is logically equal to [s1] or to [s2], then
     the result is physically equal to [s1] or to [s2].
     The height-balanced tree implementation ({!Bistro.H}) does
     not offer this guarantee.

     Time complexity: {i O(m.log (n/m))},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val union : set -> set -> set

  (**[inter s1 s2] returns the intersection of the sets [s1] and [s2],
     that is, a set that contains the common elements of the sets [s1]
     and [s2].

     The weight-balanced-tree implementation ({!Bistro.W}) offers
     the following guarantee:
     if the result is logically equal to [s1] or to [s2], then
     the result is physically equal to [s1] or to [s2].
     The height-balanced tree implementation ({!Bistro.H}) does
     not offer this guarantee.

     Time complexity: {i O(m.log (n/m))},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val inter : set -> set -> set

  (**[diff s1 s2] returns the difference of the sets [s1] and [s2],
     that is, a set that contains the elements of the set [s1]
     that do not appear in the set [s2].

     if the result is logically equal to [s1], then
     the result is physically equal to [s1].

     Time complexity: {i O(m.log (n/m))},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val diff : set -> set -> set

  (**[xor s1 s2] returns the symmetric difference of the sets [s1] and [s2],
     that is, a set that contains the elements of the set [s1]
     that do not appear in the set [s2]
     and the elements of the set [s2]
     that do not appear in the set [s1].

     Time complexity: {i O(m.log (n/m))},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val xor : set -> set -> set

  (**[split x s] returns a triple [(l, present, r)], where
     [l] is the set of the elements of [s] that are strictly less than [x],
     [r] is the set of the elements of [s] that are strictly greater than [x],
     and [present] is [true] if and only if [x] is a member of the set [s].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val split : elt -> set -> set * bool * set

  (** {1:query Querying sets} *)

  (**[is_empty s] determines whether the set [s] is the empty set.

     Time complexity: {i O(1)}. *)
  val is_empty : set -> bool

  (**If the set [s] is nonempty, [min_elt s] returns the minimum
     element of this set. Otherwise, it raises [Not_found].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val min_elt : set -> elt

  (**If the set [s] is nonempty, [min_elt_opt s] returns [Some x],
     where [x] is the minimum element of this set. Otherwise, it
     returns [None].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val min_elt_opt : set -> elt option

  (**If the set [s] is nonempty, [max_elt s] returns the maximum
     element of this set. Otherwise, it raises [Not_found].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val max_elt : set -> elt

  (**If the set [s] is nonempty, [max_elt_opt s] returns [Some x],
     where [x] is the maximum element of this set. Otherwise, it
     returns [None].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val max_elt_opt : set -> elt option

  (**If the set [s] is nonempty, [choose s] returns an arbitrary
     element of this set. Otherwise, it raises [Not_found].

     [choose] respects equality: if the sets [s1] and [s2] are equal
     then [choose s1] and [choose s2] are equal.

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val choose : set -> elt

  (**If the set [s] is nonempty, [choose_opt s] returns [Some x],
     where [x] is an arbitrary element of this set.
     Otherwise, it returns [None].

     [choose_opt] respects equality: if the sets [s1] and [s2] are equal
     then [choose_opt s1] and [choose_opt s2] are equal.

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val choose_opt : set -> elt option

  (**[mem x s] determines whether the element [x] is a member of the
     set [s].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val mem : elt -> set -> bool

  (**If [x] is a member of the set [s], then [find x s] returns the unique
     element [x'] of the set [s] such that [x] and [x'] are equivalent. (We
     say that [x] and [x'] are equivalent if [compare x x' = 0] holds.)
     Otherwise, it raises [Not_found].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val find : elt -> set -> elt
    (* The specification of [find] feels weird, because we have assumed and
       indicated everywhere that [compare] must be an order, as opposed to a
       preorder. Yet, [find] is useful only if [compare] is a preorder, so
       that the equivalence relation [compare x x' = 0] is coarser than
       equality. *)

  (**If [x] is a member of the set [s], then [find_opt x s] returns [Some x'],
     where [x'] is the unique element of the set [s] such that [x] and [x']
     are equivalent. (We say that [x] and [x'] are equivalent if
     [compare x x' = 0] holds.) Otherwise, it returns [None].

     Time complexity: {i O(log n)},
     where {i n} is the size of the set [s]. *)
  val find_opt : elt -> set -> elt option

  (**[disjoint s1 s2] determines whether the sets [s1] and [s2] are
     disjoint, that is, whether their intersection is empty. It is
     equivalent to [is_empty (inter s1 s2)].

     Time complexity: {i O(m.log (n/m))},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val disjoint : set -> set -> bool

  (**[subset s1 s2] determines whether the set [s1] is a subset of the set
     [s2], that is, whether their difference is empty. It is equivalent to
     [is_empty (diff s1 s2)].

     Time complexity: {i O(m.log (n/m))},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val subset : set -> set -> bool

  (**[equal s1 s2] determines whether the sets [s1] and [s2] are equal, that
     is, whether their symmetric difference is empty. It is equivalent to
     [is_empty (xor s1 s2)].

     Time complexity: {i O(m)},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val equal : set -> set -> bool

  (**[compare] is a total ordering function over sets. (Which specific
     ordering is used is unspecified.)

     Time complexity: {i O(m)},
     where {i m} is the size of the smaller set
     and {i n} is the size of the larger set. *)
  val compare : set -> set -> int

  (**[cardinal s] returns the cardinal of the set [s], that is,
     the number of its elements.

     Time complexity:
     in the weight-balanced-tree implementation ({!Bistro.W}),
       {i O(1)};
     in the height-balanced-tree implementation ({!Bistro.H}),
       {i O(n)},
     where {i n} is the size of the set [s]. *)
  val cardinal : set -> int

  (** {1:conversions Conversions to and from sets} *)

  (**[of_list xs] constructs a set whose elements are the elements
     of the list [xs].

     This function has adaptive time complexity.
     In the worst case, its complexity is {i O(n.log n)},
       where {i n} is the length of the list [xs].
     However, if the list [xs] is sorted,
     then its complexity is only {i O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_list : elt list -> set

  (**[to_list s] constructs a list whose elements are the elements
     of the set [s], in increasing order.

     Time complexity: {i O(n)},
     where {i n} is the size of the set [s]. *)
  val to_list : set -> elt list

  (**[elements] is a synonym for [to_list]. *)
  val elements : set -> elt list

  (**[of_array xs] constructs a set whose elements are the elements
     of the array [xs].

     This function has adaptive time complexity.
     In the worst case, its complexity is {i O(n.log n)},
       where {i n} is the length of the array [xs].
     However, if the array [xs] is sorted,
     then its complexity is only {i O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_array : elt array -> set

  (**[to_array s] constructs an array whose elements are the elements
     of the set [s], in increasing order.

     Time complexity: {i O(n)},
     where {i n} is the size of the set [s]. *)
  val to_array : set -> elt array

  (**[of_seq xs] constructs a set whose elements are the elements of the
     sequence [xs]. (The whole sequence is immediately consumed.)

     This function has adaptive time complexity.
     In the worst case, its complexity is {i O(n.log n)},
       where {i n} is the length of the list [xs].
     However, if the sequence [xs] is sorted,
     then its complexity is only {i O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_seq : elt Seq.t -> set

  (**[add_seq xs s] constructs a set whose elements are the elements
     of the sequence [xs] and the elements of the set [s].

     It is equivalent to [union (of_seq xs) s].

     Its time complexity is the combined time complexity of [of_seq] and
     [union]. *)
  val add_seq : elt Seq.t -> set -> set

  (**[to_seq s] constructs an increasing sequence whose elements are the
     elements of the set [s].

     The time complexity of consuming the entire sequence is {i O(n)},
     where {i n} is the size of the set [s]. The time complexity of
     demanding one element is {i O(log n)}. *)
  val to_seq : set -> elt Seq.t

  (**[to_seq_from x s] constructs an increasing sequence whose elements are
     the elements of the set [s] that are greater than or equal to [x].

     The time complexity of consuming the entire sequence is {i O(n)},
     where {i n} is the size of the set [s]. The time complexity of
     demanding one element is {i O(log n)}. *)
  val to_seq_from : elt -> set -> elt Seq.t

  (**[to_rev_seq s] constructs a decreasing sequence whose elements are the
     elements of the set [s].

     The time complexity of consuming the entire sequence is {i O(n)},
     where {i n} is the size of the set [s]. The time complexity of
     demanding one element is {i O(log n)}. *)
  val to_rev_seq : set -> elt Seq.t

  (** {1:iter Iterating, searching, transforming sets} *)

  val iter: (elt -> unit) -> set -> unit
  val fold: (elt -> 'a -> 'a) -> set -> 'a -> 'a
  val for_all: (elt -> bool) -> set -> bool
  val exists: (elt -> bool) -> set -> bool

  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option

  val map : (elt -> elt) -> set -> set
  val filter_map : (elt -> elt option) -> set -> set
  val filter : (elt -> bool) -> set -> set
  val partition : (elt -> bool) -> set -> set * set

  (** {1:random Random access} *)
  (* The random access functions -- not implemented by height-balanced trees. *)
  val get : set -> int -> elt
  val index : elt -> set -> int
  val split_at_2 : set -> int -> set * set
  val split_at_3 : set -> int -> set * elt * set

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

    val from_enum : elt -> set -> enum
    (** [from_enum x s] returns an enumeration of the subset of [s]
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

    val length : enum -> int
    (**[length e] returns the length of the enumeration [e]. If {!cardinal}
       has constant time complexity, then {!length} has logarithmic time
       complexity. Otherwise, {!length} has linear time complexity. *)

  end (* Enum *)

  (**/**)
  (* The function [check] is used while testing the library.
     If the library is built in release mode, this function
     has no effect. *)
  val check : set -> unit
  (**/**)

end
