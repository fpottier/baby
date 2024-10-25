# Changes

## 2024/MM/DD

* Documentation: in the signature `OrderedType`, clarify the specification
  of the function `compare`; this function decides a total preorder `≤`.

* Documentation: in the preamble, clarify that, most of the time, we assume
  that `≤` is total order; if an operation must be understood in the more
  general case where `≤` is a total preorder, then this is explicitly
  indicated.

* Documentation: update the documentation of `find` and `find_opt`
  in accordance with the previous point.

* Bug: due to a copy-paste mistake, the submodule `Baby.H.Set.Int`
  mistakenly offered weight-balanced trees
  instead of height-balanced trees. Fixed.

## 2024/06/19

* Initial implementation and release. The library offers both height-balanced
  and weight-balanced binary search trees. Only the `Set` API is supported.

* The library includes two specialized implementations of sets of integers,
  `Baby.W.Set.Int` and `Baby.H.Set.Int`. With the vanilla OCaml compiler,
  they seem about 20% faster than `Baby.W.Set.Make(Int)` and
  `Baby.H.Set.Make(Int)`. With the `flambda` OCaml compiler, there is no
  difference in performance, as this compiler is capable of inlining functor
  applications.
