# To Do

* Remove all TODOs in the code.

* Reduce the duplication between HeightBalanced, WeightBalanced,
  and the general case.

* Benchmark weight-balanced trees. (Vary α.)

* Why is my `inter` sometimes slower than the reference?
  The only difference seems to be which tree is split
    (the arguments are exchanged!).
  This suggests that maybe we should choose which tree we split
  based on its apparent size. And (when trying to preserve sharing)
  we should choose which tree we try to preserve, based on its
  apparent size.

* Could preserve sharing in `union` and `inter`.

* Should `inter`, `union`, etc. have a fast path based on physical equality?
  (I.e., if the arguments are physically equal then there is nothing to do.)
  Should this fast path be used only at the top level or also in depth?

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
