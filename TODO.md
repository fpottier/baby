# To Do

# Benchmark and minor optimizations

* Create a cleaner benchmark,
  which tests many sizes,
  and produces visual output,
  as in `hachis`.

* Benchmark (some operations on) maps.
  Compare with OCaml's standard library.

* Benchmark each specialized version of `merge`
  (namely `union`, `inter`, `diff`, `xor`)
  versus its emulation via `merge`.

* In `xor`, the singleton case can be optimized.

* In `union` (map variant), perhaps the special case where `t2` is a singleton
  is not useful, as it is unlikely to be exercised (and it does not exist in
  the set variant).

* Test `iter` and `fold` and `map` and `mapi`.
  Test that the keys are yielded in increasing order
  (when this is guaranteed by the spec).

* Check where `[@tail_mod_cons]` might be used. Benchmark it.

* Extend `of_sorted_unique_array_slice` to go beyond size 3 (`tripleton`).
  Benchmark and find out whether this makes a difference in performance.

* In the set variant,
  when `union` calls `add` (in the singleton case),
  we already have a singleton tree at hand,
  so we could pass it to `add`
  in the hope of saving one allocation.

* Every time we call `join_siblings`, we recompute balancing information that
  we already have. (The one exception is `of_sorted_unique_array_slice`.)
  Could we avoid this by passing a witness (the balancing information)?
  This would require exposing the balancing information in the type `view`
  and in variants of the view macro (`NODE`, etc.).

# New Operations

* In `union` on maps,
  do we want a specialized monomorphic variant
  that preserves sharing (as does `union` on sets)?

* In `union` on maps,
  do we want a more efficient variant (say `fusion`)
  where `f` has type `'a -> 'a -> 'a`?

* In `inter` (map variant),
  if `f` returns its second argument, we get `restrict m1 m2`,
  which restricts `m2` to the domain of `m1`.
  Should we document this fact?
  Should we define `restrict` as a specialized version of `inter`
  and publish it?

# Maybe Later

* Explore parallel computation on top of `domainslib`.
  Make `of_array` parallel.
  Add `reduce` and `map_reduce`.

* In `Weight.cppo.ml`, clarify the precondition of `balance_right_heavy`.
  Wassel Bousmaha's work contains the answer.

* Implement a variant of `choose` that runs in time *O(1)*,
  and does not promise to respect equality of sets.

* Implement `extract_min` as a thin wrapper on top of
  `min_elt` and `remove_min_elt`. (Also `extract_max`.)
  (Also `extract_min_opt` and `extract_max_opt`.)

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
