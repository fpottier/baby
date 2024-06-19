The subdirectory `WeightBalanced` contains a unit test for weight-balanced
trees.

The subdirectory `HeightBalanced` contains a unit test for height-balanced
trees.

The test code is in fact mostly shared: the file `HeightBalanced/test.ml` is a
symbolic link to `WeightBalanced/test.ml`. The differences between the two tests
appear in the files `test/{HeightBalanced,WeightBalanced}/candidate/Candidate.ml`.

Each unit test is implemented using Monolith.

Each unit test is run by descending into the appropriate subdirectory
and by typing `make random`
or (if `afl-fuzz` is available) by typing `make test`.

It is also possible to run both tests at once (in parallel) by moving
to the repository's root directory and by typing `make test`.

The subdirectory `StaticCompatibility` is a static test: its purpose is to
ensure that the public interface offered by `baby` subsumes the public
interface of OCaml's standard `Set` library. Running this test is not
necessary; compiling it is sufficient.
