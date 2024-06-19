(******************************************************************************)
(*                                                                            *)
(*                                    Baby                                    *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(**The signature [Baby.OrderedType] describes a type equipped
   with a total ordering function. *)
module type OrderedType = sig

  (**The type of the set elements. *)
  type t

  (** A total ordering function over values of type [t].
      [compare x1 x2] must be zero if [x1] and [x2] are equal.
      It must be strictly negative if [x1] is smaller than [x2].
      It must be strictly positive if [x1] is greater than [x2]. *)
  val compare : t -> t -> int

end (* OrderedType *)

(**The signature [Baby.CORE] describes the interface that must be offered by
   the balancing code to the rest of the balanced binary search tree library.
   Most operations on binary search tree are built on top of this interface,
   and are oblivious to the balancing criterion. *)
module type CORE = sig

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

  (**If the weight of a tree can be determined in constant time, then
     [weight t] returns the weight of the tree [t]. If the weight of a
     tree cannot be efficiently determined, then it is acceptable for
     [weight] to always return zero. The function [weight] is used to
     implement fast paths in subset and equality tests: it must be the
     case that [subset t1 t2] implies [weight t1 <= weight t2]. *)
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

  (**[check t] checks that the tree [t] is well-formed: that is, [t] is a
     balanced binary search tree. This function is used while testing only. *)
  val check : tree -> unit

end (* CORE *)

(**The signature [Baby.SET] describes an abstract data type of sets,
   equipped with a wide array of efficient operations. *)
module type SET = sig

  (**The type of elements. *)
  type elt

  (**The abstract type of sets. A set is an immutable data structure. *)
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

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val add : elt -> set -> set

  (**[remove x s] returns a set that contains all elements of the set
     [s], except [x].
     It is equivalent to [diff s (singleton x)].

     If the result is logically equal to [s], then
     the result is physically equal to [s].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val remove : elt -> set -> set

  (**If the set [s] is nonempty, then [remove_min_elt s] returns the
     set [s], deprived of its minimum element. Otherwise, it raises
     [Not_found].

     It is equivalent to [remove (min_elt s) s].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val remove_min_elt : set -> set

  (**If the set [s] is nonempty, then [remove_max_elt s] returns the
     set [s], deprived of its maximum element. Otherwise, it raises
     [Not_found].

     It is equivalent to [remove (max_elt s) s].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val remove_max_elt : set -> set

  (**[union s1 s2] returns the union of the sets [s1] and [s2], that
     is, a set that contains all elements of the set [s1] and all
     elements of the set [s2].

     The weight-balanced-tree implementation ({!Baby.W}) offers
     the following guarantee:
     if the result is logically equal to [s1] or to [s2], then
     the result is physically equal to [s1] or to [s2].
     The height-balanced tree implementation ({!Baby.H}) does
     not offer this guarantee.

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val union : set -> set -> set

  (**[inter s1 s2] returns the intersection of the sets [s1] and [s2],
     that is, a set that contains the common elements of the sets [s1]
     and [s2].

     The weight-balanced-tree implementation ({!Baby.W}) offers
     the following guarantee:
     if the result is logically equal to [s1] or to [s2], then
     the result is physically equal to [s1] or to [s2].
     The height-balanced tree implementation ({!Baby.H}) does
     not offer this guarantee.

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val inter : set -> set -> set

  (**[diff s1 s2] returns the difference of the sets [s1] and [s2],
     that is, a set that contains the elements of the set [s1]
     that do not appear in the set [s2].

     if the result is logically equal to [s1], then
     the result is physically equal to [s1].

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val diff : set -> set -> set

  (**[xor s1 s2] returns the symmetric difference of the sets [s1] and [s2],
     that is, a set that contains the elements of the set [s1]
     that do not appear in the set [s2]
     and the elements of the set [s2]
     that do not appear in the set [s1].

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val xor : set -> set -> set

  (**[split x s] returns a triple [(l, present, r)], where
     [l] is the set of the elements of [s] that are strictly less than [x],
     [r] is the set of the elements of [s] that are strictly greater than [x],
     and [present] is [true] if and only if [x] is a member of the set [s].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val split : elt -> set -> set * bool * set

  (** {1:query Querying sets} *)

  (**[is_empty s] determines whether the set [s] is the empty set.

     Time complexity: {m O(1)}. *)
  val is_empty : set -> bool

  (**If the set [s] is nonempty, [min_elt s] returns the minimum
     element of this set. Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val min_elt : set -> elt

  (**If the set [s] is nonempty, [min_elt_opt s] returns [Some x],
     where [x] is the minimum element of this set. Otherwise, it
     returns [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val min_elt_opt : set -> elt option

  (**If the set [s] is nonempty, [max_elt s] returns the maximum
     element of this set. Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val max_elt : set -> elt

  (**If the set [s] is nonempty, [max_elt_opt s] returns [Some x],
     where [x] is the maximum element of this set. Otherwise, it
     returns [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val max_elt_opt : set -> elt option

  (**If the set [s] is nonempty, [choose s] returns an arbitrary
     element of this set. Otherwise, it raises [Not_found].

     [choose] respects equality: if the sets [s1] and [s2] are equal
     then [choose s1] and [choose s2] are equal.

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val choose : set -> elt

  (**If the set [s] is nonempty, [choose_opt s] returns [Some x],
     where [x] is an arbitrary element of this set.
     Otherwise, it returns [None].

     [choose_opt] respects equality: if the sets [s1] and [s2] are equal
     then [choose_opt s1] and [choose_opt s2] are equal.

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val choose_opt : set -> elt option

  (**[mem x s] determines whether the element [x] is a member of the
     set [s].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val mem : elt -> set -> bool

  (**If [x] is a member of the set [s], then [find x s] returns the unique
     element [x'] of the set [s] such that [x] and [x'] are equivalent. (We
     say that [x] and [x'] are equivalent if [compare x x' = 0] holds.)
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
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

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_opt : elt -> set -> elt option

  (**[disjoint s1 s2] determines whether the sets [s1] and [s2] are
     disjoint, that is, whether their intersection is empty. It is
     equivalent to [is_empty (inter s1 s2)].

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val disjoint : set -> set -> bool

  (**[subset s1 s2] determines whether the set [s1] is a subset of the set
     [s2], that is, whether their difference is empty. It is equivalent to
     [is_empty (diff s1 s2)].

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val subset : set -> set -> bool

  (**[equal s1 s2] determines whether the sets [s1] and [s2] are equal, that
     is, whether their symmetric difference is empty. It is equivalent to
     [is_empty (xor s1 s2)].

     Time complexity: {m O(m)},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val equal : set -> set -> bool

  (**[compare] is a total ordering function over sets. (Which specific
     ordering is used is unspecified.)

     Time complexity: {m O(m)},
     where {m m} is the size of the smaller set
     and {m n} is the size of the larger set. *)
  val compare : set -> set -> int

  (**[cardinal s] returns the cardinal of the set [s], that is,
     the number of its elements.

     Time complexity:
     in the weight-balanced-tree implementation ({!Baby.W}),
       {m O(1)};
     in the height-balanced-tree implementation ({!Baby.H}),
       {m O(n)},
     where {m n} is the size of the set [s]. *)
  val cardinal : set -> int

  (** {1:conversions Conversions to and from sets} *)

  (**[of_list xs] constructs a set whose elements are the elements
     of the list [xs].

     This function has adaptive time complexity.
     In the worst case, its complexity is {m O(n.\log n)},
       where {m n} is the length of the list [xs].
     However, if the list [xs] is sorted,
     then its complexity is only {m O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_list : elt list -> set

  (**[to_list s] constructs a list whose elements are the elements
     of the set [s], in increasing order.

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val to_list : set -> elt list

  (**[elements] is a synonym for [to_list]. *)
  val elements : set -> elt list

  (**[of_array xs] constructs a set whose elements are the elements
     of the array [xs].

     This function has adaptive time complexity.
     In the worst case, its complexity is {m O(n.\log n)},
       where {m n} is the length of the array [xs].
     However, if the array [xs] is sorted,
     then its complexity is only {m O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_array : elt array -> set

  (**[to_array s] constructs an array whose elements are the elements
     of the set [s], in increasing order.

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val to_array : set -> elt array

  (**[of_seq xs] constructs a set whose elements are the elements of the
     sequence [xs]. (The whole sequence is immediately consumed.)

     This function has adaptive time complexity.
     In the worst case, its complexity is {m O(n.\log n)},
       where {m n} is the length of the list [xs].
     However, if the sequence [xs] is sorted,
     then its complexity is only {m O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_seq : elt Seq.t -> set

  (**[add_seq xs s] constructs a set whose elements are the elements
     of the sequence [xs] and the elements of the set [s].

     It is equivalent to [union (of_seq xs) s].

     Its time complexity is the combined time complexity of [of_seq] and
     [union]. *)
  val add_seq : elt Seq.t -> set -> set

  (**[to_seq s] constructs a (persistent) increasing sequence whose elements
     are the elements of the set [s].

     The time complexity of consuming the entire sequence is {m O(n)}, where
     {m n} is the size of the set [s]. The worst-case time complexity of
     demanding one element is {m O(\log n)}. *)
  val to_seq : set -> elt Seq.t

  (**[to_seq_from x s] constructs a (persistent) increasing sequence whose
     elements are the elements of the set [s] that are greater than or equal
     to [x].

     The time complexity of consuming the entire sequence is {m O(n)},
     where {m n} is the size of the set [s]. The time complexity of
     demanding one element is {m O(\log n)}. *)
  val to_seq_from : elt -> set -> elt Seq.t

  (**[to_rev_seq s] constructs a (persistent) decreasing sequence whose
     elements are the elements of the set [s].

     The time complexity of consuming the entire sequence is {m O(n)},
     where {m n} is the size of the set [s]. The time complexity of
     demanding one element is {m O(\log n)}. *)
  val to_rev_seq : set -> elt Seq.t

  (** {1:iter Iterating, searching, transforming sets} *)

  (**[iter yield s] produces an increasing sequence whose elements are
     the elements of the set [s].

     This is achieved by applying the function [yield] in turn to each
     element of the sequence.

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val iter: (elt -> unit) -> set -> unit

  (**[fold yield s accu] produces an increasing sequence whose elements
     are the elements of the set [s].

     This is achieved by applying the function [yield] in turn to each
     element of the sequence. An accumulator, whose initial value is
     [accu], is threaded through this sequence of function invocations.

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val fold: (elt -> 'a -> 'a) -> set -> 'a -> 'a

  (**[for_all p s] tests whether all elements [x] of the set [s]
     satisfy [p x = true].

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val for_all: (elt -> bool) -> set -> bool

  (**[exists p s] tests whether at least one element [x] of the set [s]
     satisfies [p x = true].

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val exists: (elt -> bool) -> set -> bool

  (**[find_first f s] requires the function [f] to be a monotonically
     increasing function of elements to Boolean values. It returns the
     least element [x] of the set [s] such that [f x] is [true], if
     there is such an element. If there is none, it raises [Not_found].

     In other words, when the elements of the set are enumerated as an
     increasing sequence, [find_first f s] returns the {i first} element
     of the sequence that follows the threshold of the function [f].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_first : (elt -> bool) -> t -> elt

  (**[find_first_opt f s] requires the function [f] to be a monotonically
     increasing function of elements to Boolean values. It returns [Some x],
     where [x] is the least element of the set [s] such that [f x] is
     [true], if there is such an element. If there is none, it returns
     [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_first_opt : (elt -> bool) -> t -> elt option

  (**[find_last f s] requires the function [f] to be a monotonically
     decreasing function of elements to Boolean values. It returns the
     greatest element [x] of the set [s] such that [f x] is [true], if
     there is such an element. If there is none, it raises [Not_found].

     In other words, when the elements of the set are enumerated as an
     increasing sequence, [find_last f s] returns the {i last} element
     of the sequence that precedes the threshold of the function [f].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_last : (elt -> bool) -> t -> elt

  (**[find_last_opt f s] requires the function [f] to be a monotonically
     decreasing function of elements to Boolean values. It returns [Some x],
     where [x] is the greatest element of the set [s] such that [f x] is
     [true], if there is such an element. If there is none, it returns
     [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_last_opt : (elt -> bool) -> t -> elt option

  (**[map f s] computes the image of the set [s] through the function [f],
     that is, in mathematical notation,
     the set {m \{ y \mid y = f(x) \wedge x \in s \}}.

     If, for every element [x] of the set [s],
     [f x] is [x],
     then the result is physically equal to [s].

     If the function [f] is monotonically increasing,
     then the time complexity is
     {m O(n)}, where {m n} is the size of the set [s];
     otherwise, it is {m O(n.\log n)}. *)
  val map : (elt -> elt) -> set -> set

  (**[filter p s] returns the set of the elements [x] of the set [s]
     such that [p x] is [true].

     If, for every element [x] of the set [s],
     [p x] is [true],
     then the result of [filter p s] is physically equal to [s].

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val filter : (elt -> bool) -> set -> set

  (**[filter_map f s] computes
     the set {m \{ y \mid \mathit{Some}\; y = f(x) \wedge x \in s \}}.

     If, for every element [x] of the set [s],
     [f x] is [Some x],
     then the result is physically equal to [s].

     If the function [f]
     (restricted to the elements that it retains)
     is monotonically increasing,
     then the time complexity is
     {m O(n)}, where {m n} is the size of the set [s];
     otherwise, it is {m O(n.\log n)}. *)
  val filter_map : (elt -> elt option) -> set -> set

  (**[partition p s] returns a pair [(s1, s2)], where
     [s1] is the set of the elements [x] of [s] such that
     [p x] is [true], and [s2] is the set of the elements of
     [s] such that [p x] is [false].

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val partition : (elt -> bool) -> set -> set * set

  (** {1:random Random access} *)

  (**{b Caution:} the following functions exist only in the
      weight-balanced-tree implementation ({!Baby.W}).
     In the height-balanced tree implementation ({!Baby.H}),
     they raise an exception of the form [Failure _]. *)

  (**In the following descriptions, a set is viewed as an increasing
     sequence of elements. Thus, the {i index} of an element is its
     index in the sequence. The valid indices in a set [s] are the
     integers in the semi-open interval of [0] to [cardinal s]. *)

  (**[get s i] requires [0 <= i && i < cardinal s]. It returns
     the element that lies at index [i] in the set [s].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val get : set -> int -> elt

  (**If [x] is a member of the set [s], then [index x s] returns the index
     [i] where this element lies in the set [s]. (Thus, [get s i] is [x].)
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val index : elt -> set -> int

  (**[cut s i] requires [0 <= i && i <= cardinal s]. It returns a pair
     [(s1, s2)], where [s1] is the set of the elements of [s] whose index
     is less than [i], and [s2] is the set of the elements of [s] whose
     index is greater than or equal to [i].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val cut : set -> int -> set * set

  (**[cut_and_get s i] requires [0 <= i && i < cardinal s]. It returns a
     triple [(s1, x, s2)], where [s1] is the set of the elements of [s]
     whose index is less than [i], [x] is the element of [s] at index [i],
     and [s2] is the set of the elements of [s] whose index is greater
     than [i].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val cut_and_get : set -> int -> set * elt * set

  (** {1:enum Enumerations} *)

  (**The submodule {!Enum} offers an abstract data type of {i enumerations},
     which allow efficient iteration over a set. *)
  module Enum : sig

    (**The type of enumerations. An enumeration an immutable data structure.

       An enumeration represents an increasing sequence of elements. It can
       also be thought of as a set.

       Enumerations and sets represent the same abstract object (namely, a
       mathematical set), but have different internal representations, and
       support a different array of operations. In particular, enumerations
       support {!head}, {!tail}, and {!from}, which allow efficient
       iteration. *)
    type enum

    (**The type [t] is a synonym for the type [enum]. *)
    type t = enum

    (**[empty] is the empty enumeration. *)
    val empty : enum

    (**[is_empty e] determines whether the enumeration [e] is empty.

       Time complexity: {m O(1)}. *)
    val is_empty : enum -> bool

    (**[enum s] returns an enumeration of the set [s]. This enumeration
       can be thought of as an increasing sequence whose elements are
       the elements of the set [s].

       Time complexity: {m O(\log n)},
       where {m n} is the size of the set [s]. *)
    val enum : set -> enum

    (**[from_enum x s] returns an enumeration whose elements are the
       elements of the set [s] that are greater than or equal to [x].
       It is equivalent to [from x (enum s)].

       Time complexity: {m O(\log n)},
       where {m n} is the size of the set [s]. *)
    val from_enum : elt -> set -> enum

    (**If the enumeration [e] is nonempty, then [head e] returns its first
       element (that is, its least element). Otherwise, it raises [Not_found].

       Time complexity: {m O(1)}. *)
    val head : enum -> elt

    (**If the enumeration [e] is nonempty, then [tail e] returns this
       enumeration, deprived of its first element (that is, its least
       element). Otherwise, it raises [Not_found].

       The worst-case time complexity of this operation is {m O(\log n)},
       where {m n} is the size of the enumeration [e].
       However, its amortized time complexity is only {m O(1)}: that is,
       the cost of enumerating all elements of a set of size {m n},
       using [head] and [tail], is only {m O(n)}. *)
    val tail : enum -> enum

    (**If the enumeration [e] is nonempty, then [head_opt e] returns its first
       element (that is, its least element). Otherwise, it returns [None].

       Time complexity: {m O(1)}. *)
    val head_opt : enum -> elt option

    (**If the enumeration [e] is nonempty, then [tail_opt e] returns this
       enumeration, deprived of its first element (that is, its least
       element). Otherwise, it returns [None].

       The worst-case time complexity of this operation is {m O(\log n)},
       where {m n} is the size of the enumeration [e].
       However, its amortized time complexity is only {m O(1)}: that is,
       the cost of enumerating all elements of a set of size {m n},
       using [head_opt] and [tail_opt], is only {m O(n)}. *)
    val tail_opt : enum -> enum option

    (**[from x e] returns the enumeration obtained from the enumeration [e]
       by skipping (removing) the elements that are less than [x].

       Time complexity: {m O(\log k)},
       where {m k} is the number of elements that are skipped. *)
    val from : elt -> enum -> enum

    (**[to_seq e] constructs a (persistent) increasing sequence whose elements
       are the elements of the enumeration [e].

       The time complexity of consuming the entire sequence is {m O(n)}, where
       {m n} is the size of the enumeration [e]. The worst-case time
       complexity of demanding one element is {m O(\log n)}. *)
    val to_seq : enum -> elt Seq.t

    (**[elements e] returns a set whose elements are the elements of the
       enumeration [e].

       Time complexity: {m O(\log n)},
       where {m n} is the size of the enumeration [e]. *)
    val elements : enum -> set

    (**[length e] returns the length of the enumeration [e],
       that is, the number of its elements.

       Time complexity:
       in the weight-balanced-tree implementation ({!Baby.W}),
         {m O(\log n)},
         where {m n} is the size of the enumeration [e];
       in the height-balanced-tree implementation ({!Baby.H}),
         {m O(n)}. *)
    val length : enum -> int

  end (* Enum *)

  (**/**)
  (* The function [check] is used while testing the library.
     If the library is built in release mode, this function
     has no effect. *)
  val check : set -> unit
  (**/**)

end (* SET *)
