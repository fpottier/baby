# Architecture of the Library

## Runtime assertions

Runtime assertions of the form `assert (...)` are used in debug mode
to check preconditions, postconditions, and invariants. These checks
can be costly. In release builds, all runtime assertions are erased.
This is controlled via the file `dune`.

The command `make test` uses the library in debug mode.

The command `make bench` uses the library in release mode.

## Arrays

The module `ArrayExtra` provides a few operations on arrays.

It is defined in the files `ArrayExtra.{ml,mli}`.

## Signatures

The module `Signatures` defines several signatures:
`OrderedType`,
`CORE`,
`SET`.

It is defined in the file `Signatures.ml`.

The file `Signatures.mli` is a symbolic link to `Signatures.ml`.
This is a way of avoiding warning 70.

## Layers

The library is organized in two layers.

### Base layer

The base layer provides
the concrete definition of the algebraic data type `tree`,
construction operations (`leaf`, `join`),
and
deconstruction operations (`view`).
The balancing logic is encapsulated in the construction operations.
This base layer obeys the signature `CORE`.

There are currently two distinct implementations of the base layer.
The file `Height.cppo.ml` offers height-balanced trees;
the file `Weight.ccpo.ml` offers weight-balanced trees.

### Upper layer

The upper layer provides
high-level operations
on sets
and is unaware of the balancing logic.

There is one implementation of the upper layer,
in the file `UpperLayer.frag.ml`.

The code of the upper layer assumes that the base-layer module has been opened,
so all base-layer types and functions are available in the current namespace.

Furthermore, the code of the upper layer assumes that the macros
`VIEW`, `LEAF`, `NODE`, `EMPTY`, and `BOTH_EMPTY` are defined.

The macros `VIEW`, `LEAF`, `NODE` are used for deconstruction only.
(For construction, `join` or other base-layer functions are used.)
These macros are used instead of the base-layer deconstruction
function `view` and can eliminate the cost of using `view` (which
involves a function call and a memory allocation).

The macros `EMPTY` and `BOTH_EMPTY` can be defined in terms of the macros
`VIEW` and `LEAF`. `EMPTY` tests whether a tree is empty (that is, a leaf);
`BOTH_EMPTY` tests whether two trees are empty.

The code of the upper layer assumes that the module `E : OrderedType`
gives the (preordered) type of the set elements (also known as keys).

## Base layer (weight balancing implementation)

The module `Weight` offers one implementation of the base layer,
using weight-balanced trees.

It is defined in the files `Weight.cppo.ml` and `Weight.mli`.

## Base layer (height balancing implementation)

The module `Height` offers one implementation of the base layer,
using height-balanced trees.

It is defined in the files `Height.cppo.ml` and `Height.mli`.

## Upper layer (implementation)

The files `*.frag.ml` contain code fragments that form the upper layer.

These fragments are assembled via `#include` into `UpperLayer.frag.ml`.

## Toplevel glue

The pieces are assembled as follows:

+ The files `ConcreteView.macros` and `AbstractView.macros` offer two
  alternative definitions of the macros `VIEW`, `LEAF`, `NODE`.

  In the concrete view, the definition of the type `tree` is known;
  no `view` function is necessary.

  In the abstract view, the type `tree` is abstract;
  the base-layer function `view` must be invoked
  in order to inspect a tree.

+ The file `Derived.macros` defines the macros `EMPTY` and `BOTH_EMPTY`
  in terms of the macros `VIEW` and `LEAF`.
  It is included both in the concrete view and in the abstract view.

+ The module `H` specializes the library for height-balanced trees.

  It uses `Height` as the base layer
  and uses the concrete view.

  It is defined in the files `H.mli` and `H.cppo.ml`.

+ The module `W` specializes the library for weight-balanced trees.

  It uses `Weight` as the base layer
  and uses the concrete view.

  It is defined in the files `W.mli` and `W.cppo.ml`.

+ The module `Baby` is the main module.

  It is defined in the files `Baby.mli` and `Baby.cppo.ml`.

  It re-exports the modules `H` and `W`
  under the names `Baby.H` and `Baby.W`.

  Furthermore, it defines the functor `Baby.Make`,
  which is parameterized over an implementation of the base layer,
  and provides the upper layer.
  This functor uses the abstract view.

  If the OCaml compiler was better at optimization,
  then the modules `Baby.H` and `Baby.W` could be obtained
  simply as instances of `Baby.Make`.
  Then, the macros `VIEW`, `LEAF`, `NODE` would disappear,
  as would the concrete view.
