# To Do

* Remove all TODOs in the code.

* Reduce the duplication between HeightBalanced, WeightBalanced,
  and the general case.

* Benchmark weight-balanced trees. (Vary α.)

* Look at `split_last` and `join2`,
  because OCaml's `concat` is implemented in a different way.

* Could preserve sharing in `union` and `inter`.

* Weight balance gives gives us constant-time cardinal,
  random access and splitting by index,
  and fast paths for `subset` and `equal`.
  It also facilitates conversion to a sorted array.

* Test `Enum`.

* Test. Eliminate the duplication between the tests in Height/Weight.

* Benchmark.

* Implement more functions from BFS.

* If possible, hide `Signatures` from the end user.

* Implement maps without code duplication.

* Explore parallel computation.
