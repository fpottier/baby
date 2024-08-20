# [make test] runs two tests in parallel, namely
# height-balanced trees and weight-balanced trees.

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  HeightBalanced \
	  WeightBalanced \

.PHONY: clean
clean:
	git clean -dfX
