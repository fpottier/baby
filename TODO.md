# To Do

* Split `Union.frag.ml` and `Readers.frag.ml` into multiple files.

* Better document the difference between `enum_from_1` and `from_more`,
  as they have the same type, but different specs.
  Rename them?

* Benchmark weight-balanced trees. (Vary α.)

* The functor `BinarySearchTree` should require `cardinal` and
  `constant_time_cardinal` instead of defining them in a pessimistic way.

* Make sure everything is tested, including `Enum`.

* Benchmark.

* Make sure that we have every function from BFS
  and from OCaml's `Set` API.

* If possible, hide `Signatures` from the end user.

* Implement maps without code duplication.

* Explore parallel computation.
