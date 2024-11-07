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
`BASE_SET`,
`BASE_MAP`,
`SET`,
`MAP`,
`SET_MAP`.

It is defined in the file `Signatures.cppo.ml`.

## Layers

The library is organized in two layers.

### Base layer

The base layer provides
the concrete definition of the algebraic data type `tree`,
construction operations (`leaf`, `join`, etc.),
and
deconstruction operations (`view`, etc.).
The balancing logic is encapsulated in the construction operations.
This base layer obeys the signature `BASE_SET` or `BASE_MAP`.

The signatures `BASE_SET` and `BASE_MAP` are identical, except `BASE_MAP`
imposes a constraint on the type parameter `'v` of the type `tree`: the
type `'v` must be a pair type `'key * 'a`.

There are currently two distinct implementations of the base layer.
The file `Height.cppo.ml` offers height-balanced trees;
the file `Weight.ccpo.ml` offers weight-balanced trees.
The file `TreeDef.frag.ml` is shared between them.

Depending on the preprocessor flag `MAP_VARIANT`,
these files implement either `BASE_SET` or `BASE_MAP`.

### Upper layer

The upper layer provides
high-level operations
on sets
and is unaware of the balancing logic.

There is one implementation of the upper layer,
in the file `UpperLayer.frag.ml`.

Depending on the preprocessor flag `MAP_VARIANT`,
the upper layer implements either sets or maps.

The code of the upper layer assumes that the base-layer module has been opened,
so all base-layer types and functions are available in the current namespace.

Furthermore, the code of the upper layer assumes that the macros
`VIEW`, `LEAF`, `NODE`, `NODENODE`, `EMPTY`, and `BOTH_EMPTY` are defined.

The macros `VIEW`, `LEAF`, `NODE`, `NODENODE` are used for deconstruction only.
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

The modules `WeightSet` and `WeightMap`
implement the base layer (for sets and for maps)
using weight-balanced trees.

These two modules have the same code,
in the file `Weight.cppo.ml`.
The preprocessor flag `MAP_VARIANT`
indicates which variant is desired.
It is undefined in the set variant;
it is defined in the map variant.

There are two distinct interface files,
`WeightSet.mli` and `WeightMap.mli`.

## Base layer (height balancing implementation)

The modules `HeightSet` and `HeightMap`
implement the base layer (for sets and maps)
using height-balanced trees.

These two modules have the same code,
in the file `Height.cppo.ml`.
The preprocessor flag `MAP_VARIANT`
indicates which variant is desired.
It is undefined in the set variant;
it is defined in the map variant.

There are two distinct interface files,
`HeightSet.mli` and `HeightMap.mli`.

## Upper layer (implementation)

The files `*.frag.ml` contain code fragments that form the upper layer.

These fragments are assembled via `#include` into `UpperLayer.frag.ml`.

The preprocessor flag `MAP_VARIANT`
indicates which variant is desired.

The file `UpperLayer.frag.ml` defines a number of macros
which can be used in the files `*.frag.ml`.

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

  It uses `HeightSet` and `HeightMap` as the base layers
  and uses the concrete view.

  It is defined in the files `H.mli` and `H.cppo.ml`.

+ The module `W` specializes the library for weight-balanced trees.

  It uses `WeightSet` and `WeightMap` as the base layers
  and uses the concrete view.

  It is defined in the files `W.mli` and `W.cppo.ml`.

+ The module `Baby` is the main module.

  It is defined in the files `Baby.mli` and `Baby.cppo.ml`.

  It re-exports the modules `H` and `W`
  under the names `Baby.H` and `Baby.W`.

  Furthermore, it defines the functor `Baby.Custom`,
  which is parameterized over two implementations of the base layer
  (for sets and for maps),
  and provides the upper layers
  (for sets and for maps).
  This functor uses the abstract view.

  If the OCaml compiler was better at optimization,
  then the modules `Baby.H` and `Baby.W` could be obtained
  simply as instances of `Baby.Custom`.
  Then, the macros `VIEW`, `LEAF`, `NODE` would disappear,
  as would the concrete view.
