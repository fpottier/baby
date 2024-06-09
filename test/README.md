The subdirectory `WeightBalanced` contains a unit test for weight-balanced
trees.

The subdirectory `HeightBalanced` contains a unit test for height-balanced
trees.

The test code is in fact shared: the file `HeightBalanced/test.cppo.ml` is a
symbolic link to `WeightBalanced/test.cppo.ml`. The preprocessor `cppo` is
used to define the symbol `HEIGHT` in one case and `WEIGHT` in the other case.

Each unit test is implemented using Monolith.

Each unit test is run by descending into the appropriate subdirectory
and by typing `make random`
or (if `afl-fuzz` is available) by typing `make test`.

It is also possible to run both tests at once (in parallel) by moving
to the repository's root directory and by typing `make test`.
