# Baby: Fast Sets Based on Balanced Binary Search Trees

`baby` is an OCaml library that offers several implementations of
balanced binary search trees.

Height-balanced and weight-balanced binary search trees are offered
out of the box. Furthermore, to advanced users, the library offers
a lightweight way of implementing other balancing strategies.

## Installation and Usage

Type `opam install baby`.

In your `dune` file, add `(libraries baby)` to the description of
your `library` or `executable`.

To use `baby`'s ready-made weight-balanced trees,
just use the functor `Baby.W.Set.Make`
instead of the usual `Set.Make`.

## Documentation

For more information,
please see the [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/baby/doc/baby/).

## Information for developers

An overview of the architecture of the library
is given in [HACKING.md](HACKING.md).
