# To Do

# Later

* Implement maps on top of sets,
  without an indirection,
  and without code duplication.

* Explore parallel computation on top of `domainslib`.
  Make `of_array` parallel.
  Add `reduce` and `map_reduce`.

# Maybe Later

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
  constructing and throwing away useless subtrees). Beside, the functions
  `find_first` and `first_last` allow searching based on a monotone predicate;
  one could also allow splitting based on a monotone predicate.

* The submodule `Enum` of increasing enumerations could be duplicated so as to
  also offer a submodule `RevEnum` of decreasing enumerations. I will wait
  until there is a need.
