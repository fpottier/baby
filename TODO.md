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

* Make sure that we have every function from BFS
  and from OCaml's `Set` API.

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

* Enumerations could support the random access functions (`get`, etc.).
