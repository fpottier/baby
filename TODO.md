# To Do

* Remove all TODOs in the code.

* Reduce the duplication between HeightBalanced, WeightBalanced,
  and the general case.

* Benchmark weight-balanced trees. (Vary α.)

* Having `is_singleton` read the children (as opposed to the height or weight)
  might result overall in more efficient union, intersection, etc. Benchmark.

* Is the complexity of `subset` and `disjoint` correct?
  Benchmark at large sizes.

* Weight balance gives gives us constant-time cardinal,
  random access and splitting by index.
  It also facilitates conversion to a sorted array.

* Make sure everything is tested, including `Enum`.

* Benchmark.

* Implement more functions from BFS.

* If possible, hide `Signatures` from the end user.

* Implement maps without code duplication.

* Explore parallel computation.
