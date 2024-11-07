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

(* -------------------------------------------------------------------------- *)

(**The signature [Baby.OrderedType] describes a type equipped
   with a total ordering function. *)
module type OrderedType = sig

  (**The type of the set elements. *)
  type t

  (**The function [compare] decides a relation {m \leq}
     over elements of type [t].

     The relation {m \leq} must be {b a total preorder}: that is,
     + for all elements {m x, y},
       it must be the case that
       {m x \leq y} or {m y \leq x} holds.
     + for all elements {m x, y, z},
       it must be the case that
       {m x \leq y} and {m y \leq z} imply {m x \leq z};

     Let us write {m x \equiv y} when
     {m x \leq y} and {m y \leq x} hold.
     In that case, we say that {m x} and {m y} are {b equivalent}.

     Let us write {m x < y} when
     {m x \leq y} and {m \neg (y \leq x)} hold.

     [compare] must behave as follows:
     + if {m x \equiv y} holds
       then [compare x y] must be zero;
     + if {m x < y} holds
       then [compare x y] must be negative;
     + if {m y < x} holds
       then [compare x y] must be positive.

     If equivalence implies equality
     (that is, if for all elements {m x, y},
       {m x \equiv y} implies {m x = y})
     then we say that the relation {m \leq} is {b a total order}. *)
  val compare : t -> t -> int

end (* OrderedType *)



(* -------------------------------------------------------------------------- *)

(* The base signature describes the interface that is offered by the base
   layer (the balancing code) to the upper layer (the set or map library).

   We define two variants of this signature, [BASE_SET] and [BASE_MAP].
   They differ only in the optional constraint that is imposed on the
   abstract type ['v tree]. In [BASE_SET], no constraint is imposed;
   in [BASE_MAP], the parameter ['v] is constrained to be a product type. *)

#def BASE_SIG(BASE, OPTIONAL_CONSTRAINT)
module type BASE = sig

  (**Balanced binary search trees. *)
  type 'v tree
    OPTIONAL_CONSTRAINT

  (**A view on a balanced binary search tree indicates whether this tree
     is a leaf or a node, and, if it is a node, gives access to its left
     child, its value, and its right child. A view does not give access
     to balancing information, such as the tree's height or weight. *)
  type 'v view =
    | Leaf
    | Node of 'v tree * 'v * 'v tree

  (**[view] turns a tree into a view. *)
  val view : 'v tree -> 'v view

  (* In the reverse direction, one could imagine a conversion function
     [make : view -> tree]. In order to avoid a memory allocation, we
     replace this function with a constant [leaf] and a function [join]. *)

  (**[leaf] is the empty tree; a leaf. *)
  val leaf : 'v tree

  (**[join l v r] expects a subtree [l], a value [v], and a subtree [r] such
     that [l < v < r] holds. It returns a new tree whose elements are the
     elements of [l], [v], and [r]. If needed, it performs rebalancing, so
     the value [v] is not necessarily found at the root of the new tree. *)
  val join : 'v tree -> 'v -> 'v tree -> 'v tree
  (* Regarding computational complexity, BFS write: the cost of [join] must be
     proportional to the difference in ranks of two trees, and the rank of the
     result of a join must be at most one more than the maximum rank of the
     two arguments. *)

  (* The following functions are not part of the minimal interface proposed
     by BFS. Exposing these functions allow us to gain efficiency in various
     situations. *)

  (**[siblings t1 t2] determines whether the trees [t1] and [t2] can be
     siblings in a well-formed tree, that is, whether they can be the
     children of a well-formed binary node (without rebalancing).
     This property is the precondition of [join_siblings]. *)
  val siblings : 'v tree -> 'v tree -> bool

  (**[join_siblings l v r] is analogous to [join l v r]. Like [join], it
     requires [l < v < r]. Furthermore, it requires the trees [l] and [r] to
     be siblings. Two trees are siblings if they could be the children of a
     well-formed binary node. *)
  val join_siblings : 'v tree -> 'v -> 'v tree -> 'v tree

  (**[join_quasi_siblings l v r] is analogous to [join l v r]. Like [join],
     it requires [l < v < r]. Furthermore, it requires the trees [l] and [r]
     to be quasi-siblings, that is, siblings where one of the trees has been
     disturbed by adding or removing one element. *)
  val join_quasi_siblings : 'v tree -> 'v -> 'v tree -> 'v tree

  (**If the weight of a tree can be determined in constant time, then
     [weight t] returns the weight of the tree [t]. If the weight of a
     tree cannot be efficiently determined, then it is acceptable for
     [weight] to always return zero. The function [weight] is used to
     implement fast paths in subset and equality tests: it must be the
     case that [subset t1 t2] implies [weight t1 <= weight t2]. *)
  val weight : 'v tree -> int

  (**[cardinal t] returns the number of elements in the tree. Depending on the
     internal representation of trees, the function [cardinal] may have time
     complexity O(1) or O(n). This is indicated by [constant_time_cardinal]. *)
  val cardinal : 'v tree -> int

  (**[constant_time_cardinal] indicates whether [cardinal] constant time
     complexity. *)
  val constant_time_cardinal : bool

  (**[singleton x] constructs a tree whose sole element is [x]. *)
  val singleton : 'v -> 'v tree

  (**[of_sorted_unique_array_slice a i j] requires the array slice defined by
     array [a], start index [i], and end index [j] to be sorted and to contain
     no duplicate elements. It converts this array slice, in linear time, to a
     tree. *)
  val of_sorted_unique_array_slice : 'v array -> int -> int -> 'v tree

  (**[seems_smaller t1 t2] indicates which of the trees [t1] and [t2] seems
     smaller, based on height or weight. This function is used as part of a
     heuristic choice, so no correctness obligation bears on it; its
     postcondition is [true]. *)
  val seems_smaller : 'v tree -> 'w tree -> bool

  (**[check t] checks that the tree [t] is well-formed: that is, [t] is a
     balanced binary search tree. This function is used while testing only. *)
  val check : 'v tree -> unit

end (* BASE *)
#enddef

(**The signature [Baby.BASE_SET] describes the interface that is offered by
   the base layer (the balancing code) to the upper layer (the set library).  *)
BASE_SIG(BASE_SET,)

(**The signature [Baby.BASE_MAP] describes the interface that is offered by
   the base layer (the balancing code) to the upper layer (the map library).  *)
BASE_SIG(BASE_MAP, constraint 'v = 'key * 'data)



(* -------------------------------------------------------------------------- *)

(**The signature [Baby.SET] describes an abstract data type of sets,
   equipped with a wide array of efficient operations. *)
module type SET = sig

  (**The type of elements. *)
  type elt

  (**The abstract type of sets. A set is an immutable data structure. *)
  type set

  (**A synonym for the type [set]. *)
  type t = set

  (**In the following,
     we usually document the behavior of each operation
     {b only in the common case where the relation {m \leq} is a total order}.
     (To find out what this means, see {!OrderedType.compare}.)

     In this common case, a value of type {!set}
     can be thought of as a set in the usual sense.

     Outside of this common case,
     the relation {m \leq} is a total preorder,
     but not a total order:
     that is,
     equivalence does not imply equality.
     In this general case,
     a value {m s} of type {!set}
     can be thought of as a set,
     with the added property that
     {i within {m s}, equivalence implies equality}:
     that is, if {m x} and {m y} are members of {m s},
     then {m x \equiv y} implies {m x = y}.
     In other words,
     {i a set {m s} contains at most one element of each equivalence class}.

     {b Some operations are useful only in the general case}
     where the relation {m \leq} is not a total order.
     This is explicitly indicated in the documentation
     of each such operation. *)

  (**Some operations are higher-order functions: they expect a function [f]
     as an argument. Examples of higher-order functions include {!val-iter},
     {!val-fold}, and {!val-map}. When we document the time complexity of a
     higher-order function, we discount the cost of the calls to [f]. *)

  (** {1:construct Constructing sets} *)

  (**[empty] is the empty set. *)
  val empty : set

  (**[singleton x] returns a set whose sole element is [x]. *)
  val singleton : elt -> set

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

     If the result is logically equal to [s1], then
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
     [l] is the set of the elements of [s] that are less than [x],
     [r] is the set of the elements of [s] that are greater than [x],
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

  (**This operation is typically useful when the relation {m \leq} is
     not a total order.

     If the set [s] contains an element [x']
     such that [x'] {m \equiv} [x] holds,
     then [find x s] returns [x'].
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find : elt -> set -> elt

  (**This operation is typically useful when the relation {m \leq} is
     not a total order.

     If the set [s] contains an element [x']
     such that [x'] {m \equiv} [x] holds,
     then [find_opt x s] returns [Some x'].
     Otherwise, it returns [None].

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

  (**[iter consume s] produces an increasing sequence whose elements are the
     elements of the set [s]. The function [consume] is applied in turn to
     each element of the sequence.

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val iter: (elt -> unit) -> set -> unit

  (**[fold consume s init] produces an increasing sequence whose elements
     are the elements of the set [s]. The function [consume] is applied in
     turn to each element of the sequence.

     A current state is threaded through this sequence of function
     invocations; each call to [consume] receives a current state [s] and
     produces an updated state [s']. The initial value of the state is
     [init]. The final value of the state is returned by [fold].

     Time complexity: {m O(n)},
     where {m n} is the size of the set [s]. *)
  val fold: (elt -> 's -> 's) -> set -> 's -> 's

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
     that follows the threshold of the function [f].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_first : (elt -> bool) -> set -> elt

  (**[find_first_opt f s] requires the function [f] to be a monotonically
     increasing function of elements to Boolean values. It returns [Some x],
     where [x] is the least element of the set [s] such that [f x] is
     [true], if there is such an element. If there is none, it returns
     [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_first_opt : (elt -> bool) -> set -> elt option

  (**[find_last f s] requires the function [f] to be a monotonically
     decreasing function of elements to Boolean values. It returns the
     greatest element [x] of the set [s] such that [f x] is [true], if
     there is such an element. If there is none, it raises [Not_found].

     In other words, when the elements of the set are enumerated as an
     increasing sequence, [find_last f s] returns the {i last} element
     that precedes the threshold of the function [f].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_last : (elt -> bool) -> set -> elt

  (**[find_last_opt f s] requires the function [f] to be a monotonically
     decreasing function of elements to Boolean values. It returns [Some x],
     where [x] is the greatest element of the set [s] such that [f x] is
     [true], if there is such an element. If there is none, it returns
     [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the set [s]. *)
  val find_last_opt : (elt -> bool) -> set -> elt option

  (**[map f s] computes the image of the set [s] through the function [f],
     that is, in mathematical notation,
     the set {m \{ y \mid y = f(x) \wedge x \in s \}}.

     The elements of the set [s] are passed to the function [f] in
     increasing order.

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

    (**The type of enumerations. An enumeration is an immutable data
       structure.

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


(* -------------------------------------------------------------------------- *)

(**The signature [Baby.MAP] describes an abstract data type of maps,
   equipped with a wide array of efficient operations. *)
module type MAP = sig

  (**The type of keys. *)
  type key

  (**A binding [(x, v)] is a pair of a key [x] and a value [v]. *)
  type 'a binding = key * 'a

  (**The abstract type of maps. A map is an immutable data structure.

     A map, also known as a dictionary, represents a finite mapping
     of keys to values. It can also be thought of as a set of bindings,
     with the property that each key appears in at most one binding.

     The parameter ['a] is the type of the values. *)
  type 'a map

  (**A synonym for the type [map]. *)
  type 'a t = 'a map

  (**In the following,
     we document the behavior of each operation
     {b only in the case where the relation {m \leq} is a total order}.
     (To find out what this means, see {!OrderedType.compare}.) *)

  (**Some operations are higher-order functions: they expect a function [f]
     as an argument. Examples of higher-order functions include {!val-iter},
     {!val-fold}, and {!val-map}. When we document the time complexity of a
     higher-order function, we discount the cost of the calls to [f]. *)

  (** {1:construct Constructing maps} *)

  (**[empty] is the empty map. *)
  val empty : 'a map

  (**[singleton x v] returns a map whose sole binding is [(x, v)]. *)
  val singleton : key -> 'a -> 'a map

  (**[add x v m] returns a map that contains all of the bindings of the map
     [m] plus the binding [(x, v)]. If for some value [v'] a binding [(x, v')]
     was already present in the map [m] then it is replaced with the binding
     [(x, v)]. If furthermore [v'] and [v] are physically equal then the map
     [m] is returned.

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val add : key -> 'a -> 'a map -> 'a map

  (**[remove x m] returns a map that contains all of the bindings of the map
     [m], except any binding for the key [x].

     If the result is logically equal to [m], then
     the result is physically equal to [m].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val remove : key -> 'a map -> 'a map

  (**[update x f m] returns a map [m'] that agrees with the map [m] at every
     key except at the key [x], whose presence and value in the map are
     transformed by the function [f]. The map [m'] is characterized by the
     following two equations:

     + [find_opt x m' = f (find_opt x m)]
     + at every key [y] other than [x],
       [find_opt y m' = find_opt y m].

     [update x f m] invokes the function [f] exactly once. If the result of
     this invocation of [f] is physically equal to its argument, then [m']
     is physically equal to [m]. *)
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  (**[add_to_list x v m] returns a map [m'] that agrees with the map [m] at
     every key except at the key [x], where the value [x] is pushed in front
     of the list that was previously associated with [x]. The map [m'] is
     characterized by the following three equations:

     + [find_opt x m' = Some [v]] if [find_opt x m] is [None],
     + [find_opt x m' = Some (v :: vs)] if [find_opt x m] is [Some vs],
     + at every key [y] other than [x],
       [find_opt y m' = find_opt y m]. *)
  val add_to_list : key -> 'a -> 'a list map -> 'a list map

  (**If the map [m] is nonempty, then [remove_min_binding m] returns the map
     [m], deprived of its minimum binding. Otherwise, it raises [Not_found].

     It is equivalent to [remove (fst (min_binding m)) m].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val remove_min_binding : 'a map -> 'a map

  (**If the map [m] is nonempty, then [remove_max_binding m] returns the map
     [m], deprived of its maximum binding. Otherwise, it raises [Not_found].

     It is equivalent to [remove (fst (max_binding m)) m].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val remove_max_binding : 'a map -> 'a map

  (**[union f m1 m2] returns a map [m] that is a form of union of the maps
     [m1] and [m2]. This map is defined as follows:

     + if a key [x] is present in the map [m1] with value [v1] and present
       in the map [m2] with value [v2], then its absence or presence (and
       value) in the map [m] are determined by the result of the function
       call [f x v1 v2], whose type is ['a option].
     + if a key [x] is absent in one map then its absence or presence (and
       value) in the map [m] are determined by the other map.

     [union] is a special case of [merge]. Indeed, [union f m1 m2] is
     logically equal to [merge f' m1 m2], where [f'] is defined as follows:
     - [f' _  None      None     = None]
     - [f' _ (Some v)   None     = Some v]
     - [f' _  None     (Some v)  = Some v]
     - [f' x (Some v1) (Some v2) = f x v1 v2]

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val union : (key -> 'a -> 'a -> 'a option) -> 'a map -> 'a map -> 'a map

  (**[inter f m1 m2] returns a map [m] that is a form of intersection of the
     maps [m1] and [m2]. This map is defined as follows:

     + if a key [x] is present in the map [m1] with value [v1] and present
       in the map [m2] with value [v2], then its absence or presence (and
       value) in the map [m] are determined by the result of the function
       call [f x v1 v2], whose type is ['a option].
     + if a key [x] is absent in [m1] or in [m2] then it is absent in the
       map [m].

     [inter] is a special case of [merge]. Indeed, [inter f m1 m2] is
     logically equal to [merge f' m1 m2], where [f'] is defined as follows:
     - [f' x (Some v1) (Some v2) = f x v1 v2]
     - [f' _  _         _        = None]

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val inter : (key -> 'a -> 'b -> 'c option) -> 'a map -> 'b map -> 'c map

  (**[diff m1 m2] returns the difference of the maps [m1] and [m2],
     that is, the restriction of the map [m1] to the {i complement}
     of the domain of the map [m2].

     If the result is logically equal to [m1], then
     the result is physically equal to [m1].

     [diff] is a special case of [merge]. Indeed, [diff m1 m2] is logically
     equal to [merge f m1 m2], where [f] is defined as follows:
     - [f _ od1  None    = od1]
     - [f _ od1 (Some _) = None]

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val diff : 'a map -> 'b map -> 'a map

  (**[xor m1 m2] returns the symmetric difference of the maps [m1] and [m2],
     that is, a map that contains the bindings of the map [m1]
     whose keys do not appear in the map [m2]
     and the bindings of the set [m2]
     whose keys do not appear in the map [m1].

     [xor] is a special case of [merge]. Indeed, [xor m1 m2] is logically
     equal to [merge f m1 m2], where [f] is defined as follows:
     - [f _ (Some v1)   None     = Some v1]
     - [f _  None      (Some v2) = Some v2]
     - [f _ (Some  _)  (Some  _) = None]
     - [f    None       None     = None]

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val xor : 'a map -> 'a map -> 'a map

  (**[merge f m1 m2] returns a map [m] that is computed based on the maps
     [m1] and [m2]. Assuming that the equation [f None None = None] holds,
     the map [m] is characterized by the following equation:
     for every key [x],
     [find_opt x m = f x (find_opt x m1) (find_opt x m2)] holds.

     In other words, the presence and value of each key [x] in the map [m]
     are determined by the result of applying [f] to the key [x] and to the
     presence and value of the key [x] in the maps [m1] and [m2].

     Time complexity: {m O(n.\log (n))},
     where {m n} is the size of the larger map. *)
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a map -> 'b map -> 'c map

  (**[split x m] returns a triple [(l, data, r)], where
     the map [l] contains the bindings of [m] whose key is less than [x],
     the map [r] contains the bindings of [m] whose key is greater than [x],
     [data] is [Some v] if [m] contains the binding [(x, v)],
     and [data] is [None] if [m] contains no binding for the key [x].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val split : key -> 'a map -> 'a map * 'a option * 'a map

  (** {1:query Querying maps} *)

  (**[is_empty m] determines whether the map [m] is empty.

     Time complexity: {m O(1)}. *)
  val is_empty : 'a map -> bool

  (**If the map [m] is nonempty, [min_binding m] returns the minimum
     binding of this map, that is, the binding whose key is minimal.
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val min_binding : 'a map -> 'a binding

  (**If the map [m] is nonempty, [min_binding_opt s] returns [Some b],
     where [b] is the minimum binding of this map, that is, the binding
     whose key is minimal. Otherwise, it returns [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val min_binding_opt : 'a map -> 'a binding option

  (**If the map [m] is nonempty, [max_binding m] returns the maximum
     binding of this map, that is, the binding whose key is maximal.
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val max_binding : 'a map -> 'a binding

  (**If the map [m] is nonempty, [max_binding_opt s] returns [Some b],
     where [b] is the maximum binding of this map, that is, the binding
     whose key is maximal. Otherwise, it returns [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val max_binding_opt : 'a map -> 'a binding option

  (**If the map [m] is nonempty, [choose m] returns an arbitrary
     binding of this map. Otherwise, it raises [Not_found].

     [choose] respects equality: if the maps [m1] and [m2] are equal
     then [choose m1] and [choose m2] are equal.

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val choose : 'a map -> 'a binding

  (**If the map [m] is nonempty, [choose_opt s] returns [Some b],
     where [b] is an arbitrary binding of this map.
     Otherwise, it returns [None].

     [choose_opt] respects equality: if the maps [m1] and [m2] are equal
     then [choose_opt m1] and [choose_opt m2] are equal.

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val choose_opt : 'a map -> 'a binding option

  (**[mem x m] determines whether the map [m] contains a binding for the
     key [x].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val mem : key -> 'a map -> bool

  (**If the map [m] contains a binding [(x, v)]
     then [find x m] returns [v].
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val find : key -> 'a map -> 'a

  (**If the map [m] contains a binding [(x, v)]
     then [find_opt x m] returns [Some v].
     Otherwise, it returns [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val find_opt : key -> 'a map -> 'a option

  (**[disjoint m1 m2] determines whether the maps [m1] and [m2] are
     disjoint, that is, whether the intersection of their domains is
     empty.

     Time complexity: {m O(m.\log (\frac{n}{m}))},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val disjoint : 'a map -> 'b map -> bool

  (**If [leq] is a preorder on values, then [sub leq] is a preorder
     on maps.

     [sub leq m1 m2] is [true] if,
     for every binding [(x, v1)] in the map [m1],
     there exists a binding [(x, v2)] in the map [m2]
     and [leq v1 v2] is [true].

     Time complexity: {m O(m)},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val sub : ('a -> 'b -> bool) -> 'a map -> 'b map -> bool

  (**If [eq] is an equality test over values, then [equal eq] is an
     equality test over maps.

     Time complexity: {m O(m)},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val equal : ('a -> 'b -> bool) -> 'a map -> 'b map -> bool

  (**If [cmp] is a total ordering function over values of type ['a], then
     [compare cmp] is a total ordering function over maps. (Which specific
     ordering is used is unspecified.)

     Time complexity: {m O(m)},
     where {m m} is the size of the smaller map
     and {m n} is the size of the larger map. *)
  val compare : ('a -> 'b -> int) -> 'a map -> 'b map -> int

  (**[cardinal m] returns the cardinal of the map [m], that is,
     the number of its bindings.

     Time complexity:
     in the weight-balanced-tree implementation ({!Baby.W}),
       {m O(1)};
     in the height-balanced-tree implementation ({!Baby.H}),
       {m O(n)},
     where {m n} is the size of the map [m]. *)
  val cardinal : 'a map -> int

  (** {1:conversions Conversions to and from maps} *)

  (**[of_list bs] constructs a map whose bindings are the elements
     of the list [bs].

     The list is read from left to right. If two bindings have the
     same key, then the rightmost binding (the one that is read last)
     takes precedence.

     This function has adaptive time complexity.
     In the worst case, its complexity is {m O(n.\log n)},
       where {m n} is the length of the list [bs].
     However, if the list [bs] is sorted,
     then its complexity is only {m O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_list : 'a binding list -> 'a map

  (**[to_list m] constructs a list whose elements are the bindings
     of the map [m], in increasing order.

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val to_list : 'a map -> 'a binding list

  (**[bindings] is a synonym for [to_list]. *)
  val bindings : 'a map -> 'a binding list

  (**[of_array bs] constructs a map whose bindings are the elements
     of the array [bs].

     The array is read from left to right. If two bindings have the
     same key, then the rightmost binding (the one that is read last)
     takes precedence.

     This function has adaptive time complexity.
     In the worst case, its complexity is {m O(n.\log n)},
       where {m n} is the length of the array [bs].
     However, if the array [bs] is sorted,
     then its complexity is only {m O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_array : 'a binding array -> 'a map

  (**[to_array m] constructs an array whose elements are the bindings
     of the map [m], in increasing order.

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val to_array : 'a map -> 'a binding array

  (**[of_seq bs] constructs a map whose elements are the bindings of the
     sequence [bs]. (The whole sequence is immediately consumed.)

     The sequence is read from left to right. If two bindings have the
     same key, then the rightmost binding (the one that is read last)
     takes precedence.

     This function has adaptive time complexity.
     In the worst case, its complexity is {m O(n.\log n)},
       where {m n} is the length of the sequence [bs].
     However, if the sequence [bs] is sorted,
     then its complexity is only {m O(n)}.
     In between these extremes, its complexity degrades gracefully. *)
  val of_seq : 'a binding Seq.t -> 'a map

  (**[add_seq bs m] extends the map [m] with the elements of the
     sequence [bs].

     It is equivalent to [union (fun _ _ v -> v) m (of_seq bs)].

     Its time complexity is the combined time complexity of [of_seq] and
     [union]. *)
  val add_seq : 'a binding Seq.t -> 'a map -> 'a map

  (**[to_seq m] constructs a (persistent) increasing sequence whose elements
     are the bindings of the map [m].

     The time complexity of consuming the entire sequence is {m O(n)}, where
     {m n} is the size of the map [m]. The worst-case time complexity of
     demanding one element is {m O(\log n)}. *)
  val to_seq : 'a map -> 'a binding Seq.t

  (**[to_seq_from x m] constructs a (persistent) increasing sequence
     whose elements are the bindings of the map [m] whose key is
     greater than or equal to [x].

     The time complexity of consuming the entire sequence is {m O(n)},
     where {m n} is the size of the map [m]. The time complexity of
     demanding one element is {m O(\log n)}. *)
  val to_seq_from : key -> 'a map -> 'a binding Seq.t

  (**[to_rev_seq m] constructs a (persistent) decreasing sequence whose
     elements are the bindings of the map [m].

     The time complexity of consuming the entire sequence is {m O(n)},
     where {m n} is the size of the map [m]. The time complexity of
     demanding one element is {m O(\log n)}. *)
  val to_rev_seq : 'a map -> 'a binding Seq.t

  (** {1:iter Iterating, searching, transforming maps} *)

  (**[iter consume m] produces an increasing sequence whose elements are the
     bindings of the map [m]. The function [consume] is applied in turn to
     each binding in the sequence. For each binding [(x, v)], the key [x]
     and the value [v] form two separate arguments to [consume].

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val iter: (key -> 'a -> unit) -> 'a map -> unit

  (**[fold consume m init] produces an increasing sequence whose elements
     are the bindings of the map [m]. The function [consume] is applied in
     turn to each binding in the sequence. For each binding [(x, v)], the
     key [x] and the value [v] form two separate arguments to [consume].

     A current state is threaded through this sequence of function
     invocations; each call to [consume] receives a current state [s] and
     produces an updated state [s']. The initial value of the state is
     [init]. The final value of the state is returned by [fold].

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val fold: (key -> 'a -> 's -> 's) -> 'a map -> 's -> 's

  (**[for_all p m] tests whether all bindings [(x, v)] in the map [m]
     satisfy [p x v = true].

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val for_all: (key -> 'a -> bool) -> 'a map -> bool

  (**[exists p m] tests whether at least one binding [(x, v)]
     in the map [m] satisfies [p x v = true].

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val exists: (key -> 'a -> bool) -> 'a map -> bool

  (**[find_first f m] requires the function [f] to be a monotonically
     increasing function of keys to Boolean values. It returns the least
     binding [(x, v)] of the map [m] such that [f x] is [true], if there
     is such a binding. If there is none, it raises [Not_found].

     In other words, when the bindings of the map are enumerated as an
     increasing sequence, [find_first f m] returns the {i first} binding
     that follows the threshold of the function [f].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val find_first : (key -> bool) -> 'a map -> 'a binding

  (**[find_first_opt f m] requires the function [f] to be a monotonically
     increasing function of keys to Boolean values. It returns [Some (x, v)],
     where [(x, v)] is the least binding of the map [m] such that [f x] is
     [true], if there is such a binding. If there is none, it returns [None].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val find_first_opt : (key -> bool) -> 'a map -> 'a binding option

  (**[find_last f m] requires the function [f] to be a monotonically
     decreasing function of keys to Boolean values. It returns the
     greatest binding [(x, v)] of the map [m] such that [f x] is [true], if
     there is such a binding. If there is none, it raises [Not_found].

     In other words, when the bindings of the map are enumerated as an
     increasing sequence, [find_last f m] returns the {i last} binding
     that precedes the threshold of the function [f].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val find_last : (key -> bool) -> 'a map -> 'a binding

  (**[find_last_opt f m] requires the function [f] to be a monotonically
     decreasing function of keys to Boolean values. It returns [Some (x, v)],
     where [(x, v)] is the greatest binding of the map [m] such that [f x] is
     [true], if there is such a binding. If there is none, it raises
     [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val find_last_opt : (key -> bool) -> 'a map -> 'a binding option

  (**[map f m] computes the image [m'] of the map [m] through the
     function [f].

     For every binding [(x, v)] in the map [m],
     there is a binding [(x, f v)] in the map [m'].

     The bindings are passed to the function [f] in increasing order.

     Time complexity:
     {m O(n)}, where {m n} is the size of the map [m]. *)
  val map : ('a -> 'b) -> 'a map -> 'b map

  (**[mapi f m] computes the image [m'] of the map [m] through the
     function [f].

     For every binding [(x, v)] in the map [m],
     there is a binding [(x, f x v)] in the map [m'].

     The bindings are passed to the function [f] in increasing order of keys.

     Time complexity:
     {m O(n)}, where {m n} is the size of the map [m]. *)
  val mapi : (key -> 'a -> 'b) -> 'a map -> 'b map

  (**[filter p m] returns a map [m'] that contains
     the bindings of [x] to [v] in the map [m]
     such that [p x v] is [true].

     If, for every binding [(x, v)] in the map [m],
     [p x v] is [true],
     then the result of [filter p m] is physically equal to [m].

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val filter : (key -> 'a -> bool) -> 'a map -> 'a map

  (**[filter_map f m] computes the image [m'] of the map [m] through the
     function [f].

     For every binding [(x, v)] in the map [m],
     if [f x v] is [Some v'], then
     the binding [(x, v')] is in the map [m'].

     Time complexity:
     {m O(n)}, where {m n} is the size of the map [m]. *)
  val filter_map : (key -> 'a -> 'b option) -> 'a map -> 'b map

  (**[partition p m] returns a pair [(m1, m2)], where
     the map [m1] contains
     the bindings of [x] to [v] in the map [m]
     such that [p x v] is [true],
     and
     the map [m2] contains
     the bindings of [x] to [v] in the map [m]
     such that [p x v] is [false].

     Time complexity: {m O(n)},
     where {m n} is the size of the map [m]. *)
  val partition : (key -> 'a -> bool) -> 'a map -> 'a map * 'a map

  (** {1:random Random access} *)

  (**{b Caution:} the following functions exist only in the
      weight-balanced-tree implementation ({!Baby.W}).
     In the height-balanced tree implementation ({!Baby.H}),
     they raise an exception of the form [Failure _]. *)

  (**In the following descriptions, a map is viewed as an increasing
     sequence of bindings. Thus, the {i index} of a binding is its
     index in the sequence. The valid indices in a map [m] are the
     integers in the semi-open interval of [0] to [cardinal m]. *)

  (**[get m i] requires [0 <= i && i < cardinal m]. It returns
     the binding that lies at index [i] in the map [m].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val get : 'a map -> int -> 'a binding

  (**If the key [x] exists in the map [m], then [index x m] returns
     the index [i] where a binding for this key lies in the map [m].
     (Thus, [get m i] is [(x, v)], where [v] is [find x m].)
     Otherwise, it raises [Not_found].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val index : key -> 'a map -> int

  (**[cut m i] requires [0 <= i && i <= cardinal m]. It returns a pair
     [(m1, m2)], where the map [m1] contains the bindings of [m] whose
     index is less than [i] and the map [m2] contains the bindings of
     [m] whose index is greater than or equal to [i].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val cut : 'a map -> int -> 'a map * 'a map

  (**[cut_and_get m i] requires [0 <= i && i < cardinal m]. It returns a
     triple [(m1, b, m2)], where the map [m1] contains the bindings of [m]
     whose index is less than [i], the binding [b] is [get m i], and the map
     [m2] contains the bindings of [m] whose index is greater than [i].

     Time complexity: {m O(\log n)},
     where {m n} is the size of the map [m]. *)
  val cut_and_get : 'a map -> int -> 'a map * 'a binding * 'a map

  (** {1:enum Enumerations} *)

  (**The submodule {!Enum} offers an abstract data type of {i enumerations},
     which allow efficient iteration over a map. *)
  module Enum : sig

    (**The type of enumerations. An enumeration is an immutable data
       structure.

       An enumeration represents an increasing sequence of bindings (that is,
       a sequence of bindings, sorted by increasing order of keys). It can
       also be thought of as a map.

       Enumerations and maps represent the same abstract object (namely, a
       mathematical map), but have different internal representations, and
       support a different array of operations. In particular, enumerations
       support {!head}, {!tail}, and {!from}, which allow efficient
       iteration. *)
    type 'a enum

    (**The type ['a t] is a synonym for the type ['a enum]. *)
    type 'a t = 'a enum

    (**[empty] is the empty enumeration. *)
    val empty : 'a enum

    (**[is_empty e] determines whether the enumeration [e] is empty.

       Time complexity: {m O(1)}. *)
    val is_empty : 'a enum -> bool

    (**[enum m] returns an enumeration of the map [m]. This enumeration
       can be thought of as an increasing sequence whose elements are
       the bindings of the map [m].

       Time complexity: {m O(\log n)},
       where {m n} is the size of the map [m]. *)
    val enum : 'a map -> 'a enum

    (**[from_enum x m] returns an enumeration whose elements are the
       bindings of the map [m] whose key is greater than or equal to [x].
       It is equivalent to [from x (enum m)].

       Time complexity: {m O(\log n)},
       where {m n} is the size of the map [m]. *)
    val from_enum : key -> 'a map -> 'a enum

    (**If the enumeration [e] is nonempty, then [head e] returns its first
       element (that is, the binding whose key is the least key). Otherwise,
       it raises [Not_found].

       Time complexity: {m O(1)}. *)
    val head : 'a enum -> 'a binding

    (**If the enumeration [e] is nonempty, then [tail e] returns this
       enumeration, deprived of its first element (that is, deprived of the
       binding whose key is the least key). Otherwise, it raises
       [Not_found].

       The worst-case time complexity of this operation is {m O(\log n)},
       where {m n} is the size of the enumeration [e].
       However, its amortized time complexity is only {m O(1)}: that is,
       the cost of enumerating all bindings of a map of size {m n},
       using [head] and [tail], is only {m O(n)}. *)
    val tail : 'a enum -> 'a enum

    (**If the enumeration [e] is nonempty, then [head_opt e] returns its
       first element (that is, the binding whose key is the least key).
       Otherwise, it returns [None].

       Time complexity: {m O(1)}. *)
    val head_opt : 'a enum -> 'a binding option

    (**If the enumeration [e] is nonempty, then [tail_opt e] returns this
       enumeration, deprived of its first element (that is, deprived of the
       binding whose key is the least key). Otherwise, it returns [None].

       The worst-case time complexity of this operation is {m O(\log n)},
       where {m n} is the size of the enumeration [e].
       However, its amortized time complexity is only {m O(1)}: that is,
       the cost of enumerating all bindings of a map of size {m n},
       using [head_opt] and [tail_opt], is only {m O(n)}. *)
    val tail_opt : 'a enum -> 'a enum option

    (**[from x e] returns the enumeration obtained from the enumeration [e]
       by skipping (removing) the bindings whose key is less than [x].

       Time complexity: {m O(\log k)},
       where {m k} is the number of bindings that are skipped. *)
    val from : key -> 'a enum -> 'a enum

    (**[to_seq e] constructs a (persistent) increasing sequence whose elements
       are the elements of the enumeration [e].

       The time complexity of consuming the entire sequence is {m O(n)}, where
       {m n} is the size of the enumeration [e]. The worst-case time
       complexity of demanding one element is {m O(\log n)}. *)
    val to_seq : 'a enum -> 'a binding Seq.t

    (**[elements e] returns a map whose bindings are the elements of the
       enumeration [e].

       Time complexity: {m O(\log n)},
       where {m n} is the size of the enumeration [e]. *)
    val elements : 'a enum -> 'a map

    (**[length e] returns the length of the enumeration [e],
       that is, the number of its elements.

       Time complexity:
       in the weight-balanced-tree implementation ({!Baby.W}),
         {m O(\log n)},
         where {m n} is the size of the enumeration [e];
       in the height-balanced-tree implementation ({!Baby.H}),
         {m O(n)}. *)
    val length : 'a enum -> int

  end (* Enum *)

  (**/**)
  (* The function [check] is used while testing the library.
     If the library is built in release mode, this function
     has no effect. *)
  val check : 'a map -> unit
  (**/**)

end (* MAP *)

(* -------------------------------------------------------------------------- *)

(**The signature [Baby.SET_MAP] describes abstract data types of sets
   and maps. They are described in two forms:

   + If the user needs just sets, or just maps, they can use the
     functors [Set.Make] and [Map.Make], which are analogous to (and
     compatible with) the functors by the same names in OCaml's standard
     library.

   + If the user needs both sets and maps, as well as conversion
     functions between sets and maps, they can use the functor
     [Make]. This functor produces a module which contains
     a submodule [Set], a submodule [Map],
     and two conversion functions, [domain] and [lift]. *)
module type SET_MAP = sig

  module Set : sig
    module type OrderedType = OrderedType
    module type S = SET
    module Make (E : OrderedType) : SET with type elt = E.t
  end

  module Map : sig
    module type OrderedType = OrderedType
    module type S = MAP
    module Make (E : OrderedType) : MAP with type key = E.t
  end

  module Make (E : OrderedType) : sig

    module Set : SET with type elt = E.t
    module Map : MAP with type key = E.t

    (**[domain m] returns the domain of the map [m], that is, the set of
       all keys for which there is a binding in the map [m].

       Time complexity: {m O(n)},
       where {m n} is the size of the map [m]. *)
    val domain : 'a Map.t -> Set.t

    (**[lift f s] returns a map whose domain is the set [s] and where
       every key [x] in the set [s] is mapped to the value [f x].

       Time complexity: {m O(n)},
       where {m n} is the size of the set [s]. *)
    val lift   : (Map.key -> 'a) -> Set.t -> 'a Map.t

  end

end
