# Changes

## 2024/06/XX

Add two specialized implementations of sets of integers, `Bistro.W.Set.Int`
and `Bistro.H.Set.Int`. With the vanilla OCaml compiler, they seem about 20%
faster than `Bistro.W.Set.Make(Int)` and `Bistro.H.Set.Make(Int)`. With the
`flambda` OCaml compiler, there is no difference in performance, as this
compiler is capable of inlining functor applications.

## 2024/06/17

Initial implementation and release. The library offers both height-balanced
and weight-balanced binary search trees. Only the `Set` API is supported.
