# Changes

## 2024/06/19

* Initial implementation and release. The library offers both height-balanced
  and weight-balanced binary search trees. Only the `Set` API is supported.

* The library includes two specialized implementations of sets of integers,
  `Baby.W.Set.Int` and `Baby.H.Set.Int`. With the vanilla OCaml compiler,
  they seem about 20% faster than `Baby.W.Set.Make(Int)` and
  `Baby.H.Set.Make(Int)`. With the `flambda` OCaml compiler, there is no
  difference in performance, as this compiler is capable of inlining functor
  applications.
