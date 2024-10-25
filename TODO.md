# To Do

# Later

* Implement maps on top of sets,
  without an indirection,
  and without code duplication.

* Explore parallel computation on top of `domainslib`.
  Make `of_array` parallel.
  Add `reduce` and `map_reduce`.

# Maybe Later

* In `Weight.cppo.ml`, clarify the precondition of `balance_right_heavy`.
  Wassel Bousmaha's work contains the answer.

* Implement a variant of `choose` that runs in time *O(1)*,
  and does not promise to respect equality of sets.

* Implement `extract_min` as a thin wrapper on top of
  `min_elt` and `remove_min_elt`. (Also `extract_max`.)
  (Also `extract_min_opt` and `extract_max_opt`.)

* One might wish to expose `view`, `join`, and `join2` to the end user, so as
  to allow her to define their own operations (with access to the tree
  structure) if desired.

* Blelloch et al. discuss multi-insert and multi-delete operations, which
  insert or remove an array of elements at once. These can be simulated by
  converting the array to a set (using `of_array`) and using a set union or
  set difference (`union`, `diff`). Some time is wasted in the conversion of
  the array to a balanced binary tree, but (for now, at least) this seems
  acceptable. `add_seq` is an example of a multi-insertion.

* Blelloch et al. offers variations on `split`, such as `range`, `up_to`,
  `down_to`, which allow selecting a subset of elements, based on ordering
  constraints. These can be simulated using `split` (at the cost of
  constructing and throwing away useless subtrees). Besides, the functions
  `find_first` and `first_last` allow searching based on a monotone predicate;
  one could also allow splitting based on a monotone predicate.

* The submodule `Enum` of increasing enumerations could be duplicated so as to
  also offer a submodule `RevEnum` of decreasing enumerations. I will wait
  until there is a need.

* In the general case where `≤` is a total preorder (not a total order), the
  current `Set` API is not quite usable; many operations are missing. To see
  which operations are missing, imagine implementing maps on top of sets: a
  map is a set of pairs, where the preorder on pairs compares just the first
  components. One can then see that (at least) the following operations are
  missing:

  + `replace`: a variant of `add` which always replaces the old element with
    the new element, and preserves physical equality when possible (that is,
    when the old and new elements are physically equal);

  + variants of `mem`, `find`, `remove`, `split`, `Enum.from` that take the
    partial application `compare x` as an argument, instead of `x` itself;

  + `update`, which also expects `compare x` as an argument, and which expects
    a transformation function `f` of type `elt option -> elt option`.
    The function `f` must preserve equivalence:
    if `f (Some x) = Some y` then `x ≡ y` must hold.
    This requirement must be enforced at runtime by a defensive check.

  + `merge`, which expects a combination function `f` of type
    `elt option -> elt option -> elt option`.
    The function `f` must preserve equivalence:
    if `x ≡ y` and `f (Some x) (Some y) = Some z`
    then `x ≡ y ≡ z` must hold.
    This requirement must be enforced at runtime by a defensive check.

  + a variant of `equal` (resp. `compare`) that is parameterized with an
    an equality function (resp. an ordering function) on elements.

  + a variant of `map` where the function `f` must preserve equivalence:
    if `f x = y` then `x ≡ y` must hold.
    This requirement must be enforced at runtime by a defensive check.
