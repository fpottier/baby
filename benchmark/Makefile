# [make all] compiles this benchmark.

.PHONY: all
all:
	@ dune build --profile release

# [make test] runs this benchmark.

# Using --profile release ensures that debug assertions are turned off.

.PHONY: test
test:
	@ dune exec --profile release \
	    --display=quiet --no-print-directory \
	     ./main.exe
