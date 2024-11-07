# Changes

## 2024/MM/DD

* The library now offers both sets and maps.
  The modules `Baby.H.Set` and `Baby.W.Set` continue to exist,
    and are compatible with OCaml's `Set` library.
  The modules `Baby.H.Map` and `Baby.W.Map` appear,
    and are compatible with OCaml's `Set` library.
  Furthermore,
  the functors `Baby.H.Make` and `Baby.W.Make` appear.
  These functors produce a module that contains sets, maps,
  and two conversion functions between sets and maps,
  namely `domain` and `lift`.

* Incompatible changes:
  + The modules `Baby.H.Set.Int` and `Baby.W.Set.Int` have been removed.
    This makes the library simpler and smaller.
    Use `Baby.H.Set.Make(Int)` and `Baby.W.Set.Make(Int)` instead.
    With the `flambda` OCaml compiler, there is no difference in performance.
  + The functor `Baby.Make` has been renamed `Baby.Custom`,
    and its signature has changed. It now requires two
    arguments whose signatures are `BASE_SET` and `BASE_MAP`.
  + The signature `CORE` has been renamed to `BASE_SET`.
    Furthermore, this signature has changed:
    - the type `key` has been removed;
    - the types `tree` and `view` are now parameterized;
    - `join_weight_balanced` has been renamed to `join_siblings;
    - `join_neighbors` has been renamed to `join_quasi_siblings`.
  + The undocumented modules `Baby.Height` and `Baby.Weight` have been removed.

* Documentation: in the signature `OrderedType`, clarify the specification
  of the function `compare`; this function decides a total preorder `≤`.

* Documentation: in the preamble, clarify that, most of the time, we assume
  that `≤` is total order; if an operation must be understood in the more
  general case where `≤` is a total preorder, then this is explicitly
  indicated.

* Documentation: update the documentation of `find` and `find_opt`
  in accordance with the previous point.

## 2024/06/19

* Initial implementation and release. The library offers both height-balanced
  and weight-balanced binary search trees. Only the `Set` API is supported.

* The library includes two specialized implementations of sets of integers,
  `Baby.W.Set.Int` and `Baby.H.Set.Int`. With the vanilla OCaml compiler,
  they seem about 20% faster than `Baby.W.Set.Make(Int)` and
  `Baby.H.Set.Make(Int)`. With the `flambda` OCaml compiler, there is no
  difference in performance, as this compiler is capable of inlining functor
  applications.
