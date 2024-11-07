The subdirectory `HeightBalanced` contains a test of height-balanced trees.

The subdirectory `WeightBalanced` contains a test of weight-balanced trees.

The code is shared: the file `HeightBalanced/test.ml` is a symbolic link to
`WeightBalanced/test.ml`. The differences between the two tests appear in the
files `test/{HeightBalanced,WeightBalanced}/candidate/Candidate.ml`.

The candidate implementation forms a library,
named `height_candidate` or `weight_candidate`.
Furthermore, some helper functions form a library,
named `helpers`.
These libraries are automatically loaded when `dune utop` is launched.
This makes it easy to reproduce a test scenario inside `dune utop`.

Each test is implemented using Monolith.

The tests are executed by one of the following commands:

```bash
make -C HeightBalanced random        # test height-balanced trees
make -C WeightBalanced random        # test weight-balanced trees
make test                            # run both in parallel
```

When a test fails, a test scenario is printed.
This scenario can be copied and pasted into the file `play.ml`
in the project's root directory.
This file can then be executed by launching `dune utop`
and typing `#use "play.ml"`.
This should be enough to reproduce the problem.

The subdirectory `StaticCompatibility` is a static test: its purpose is to
ensure that the public interface offered by `baby` subsumes the public
interface of OCaml's standard `Set` library. Running this test is not
necessary; compiling it is sufficient.
