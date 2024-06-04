# To Do

* Remove all TODOs in the code.

* Reduce the duplication between HeightBalanced, WeightBalanced,
  and the general case.

* Benchmark weight-balanced trees. (Vary α.)

* Why is my `inter` slower than the reference?

* Could preserve sharing in `union` and `inter`.

* Should `inter`, `union`, etc. have a fast path based on
  physical equality?

* Is the complexity of `subset` and `disjoint` correct?
  Benchmark at large sizes.

* Weight balance gives gives us constant-time cardinal,
  random access and splitting by index.
  It also facilitates conversion to a sorted array.

* Test `Enum`.

* Test. Eliminate the duplication between the tests in Height/Weight.

* Benchmark.

* Implement more functions from BFS.

* If possible, hide `Signatures` from the end user.

* Implement maps without code duplication.

* Explore parallel computation.
