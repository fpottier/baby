# To Do

* Name the signature `S` instead of `SET`? Or both.

* Make sure every file is clean and documented.

* Hide or clearly mark all unsafe operations.
    (`of_sorted_unique_array` should probably be hidden.)

* Benchmark weight-balanced trees. (Vary α.)

* Make sure everything is tested, including `Enum`.

* Make sure assertions are erased in release mode (ArrayExtra).

* Document what this library offers over OCaml's Set library.
  In the weight-balanced implementation,
  a constant time `cardinal` function;
  efficient random access functions;
  fast negative paths (based on cardinal) for `subset` and `equal`.
  In `union`, `inter`, and possibly a few other functions,
  better preservation of sharing.
  New function `xor`.
  In `of_list`, `of_array`, `of_seq`, adaptive complexity.
  New functions `of_array` and `to_array`.
  New functions `remove_min_elt` and `remove_max_elt`.
  Enumerations.
  Clear documentation of the time complexity of every operation.

* Document the potential departures from OCaml's Set library.
  In `of_list`, `of_seq`, `add_seq`,
  if the list or sequence contains duplicate elements,
  then which element is retained is unspecified.
  (A difference with `Set` can be observed only if the equivalence relation
   on elements is coarser than equality.)

* Implement a variant of `choose` that runs in time O(1),
  and does not promise to respect equality of sets.

* Add README, AUTHORS, LICENSE, CHANGES, etc. Add headers.

* In the documentation `Seq` shows up as `Stdlib.Seq`,
  and the hyperlink does not work.

# Later

* Implement maps on top of sets,
  without an indirection,
  and without code duplication.

* Explore parallel computation.
  Add `reduce` and `map_reduce`.

# Not To Do

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
  constructing and throwing away useless subtrees).

* The submodule `Enum` of increasing enumerations could be duplicated so as to
  also offer a submodule `RevEnum` of decreasing enumerations. I will wait
  until there is a need.
