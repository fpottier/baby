# To Do

* Remove all TODOs in the code.

* Reduce the duplication between HeightBalanced, WeightBalanced,
  and the general case.

* Benchmark weight-balanced trees. (Vary α.)

* Having `is_singleton` read the children (as opposed to the height or weight)
  might result overall in more efficient union, intersection, etc. Benchmark.

* In `join2` and `join2_siblings`,
  could choose a side based on which tree seems smaller.

* Could better preserve sharing in `union`, `diff`, `xor`.

* Is the complexity of `subset` and `disjoint` correct?
  Benchmark at large sizes.

* Weight balance gives gives us constant-time cardinal,
  random access and splitting by index.
  It also facilitates conversion to a sorted array.

* Make sure everything is tested, including `Enum`.

* Eliminate the duplication between the tests in Height/Weight.

* Update the toplevel `make test` to test both height-balanced and
  weight-balanced trees in parallel. Save a log of each test and
  show both output streams in the console.

* Benchmark.

* Implement more functions from BFS.

* If possible, hide `Signatures` from the end user.

* Implement maps without code duplication.

* Explore parallel computation.
