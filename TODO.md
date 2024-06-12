# To Do

* Make sure every file is clean and documented.

* Hide or clearly mark all unsafe operations.
    (`of_sorted_unique_array` should probably be hidden.)

* Better document the difference between `enum_from_1` and `from_more`,
  as they have the same type, but different specs.
  Rename them?

* Implement `to_rev_seq`.

* Benchmark weight-balanced trees. (Vary α.)

* Make sure everything is tested, including `Enum`.

* Make sure assertions are erased in release mode (ArrayExtra).

* Document the potential departures from OCaml's Set library.
  In `of_list`, `of_seq`, `add_seq`,
  if the list or sequence contains duplicate elements,
  then which element is retained is unspecified.
  (A difference with `Set` can be observed only if the equivalence relation
   on elements is coarser than equality.)

* Add README, AUTHORS, LICENSE, CHANGES, etc. Add headers.

* Implement maps on top of sets,
  without an indirection,
  and without code duplication.

* Explore parallel computation.
  Add `reduce` and `map_reduce`.

# Not To Do

* Blelloch et al. discuss multi-insert and multi-delete operations, which
  insert or remove an array of elements at once. These can be simulated by
  converting the array to a set (using `of_array`) and using a set union or
  set difference (`union`, `diff`). Some time is wasted in the conversion of
  the array to a balanced binary tree, but (for now, at least) this seems
  acceptable.

* Blelloch et al. offers variations on `split`, such as `range`, `up_to`,
  `down_to`, which allow selecting a subset of elements, based on ordering
  constraints. These can be simulated using `split` (at the cost of
  constructing and throwing away useless subtrees).
