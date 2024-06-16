# ------------------------------------------------------------------------------

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The project's name.
THIS     := bistro

# The archive's URL (https).
ARCHIVE  := https://github.com/fpottier/$(THIS)/archive/$(DATE).tar.gz

# ------------------------------------------------------------------------------

.PHONY: all
all:
	@ dune build @all

.PHONY: clean
clean:
	@ git clean -fdX

# [make test] tests both height-balanced and weight-balanced trees.
# The output of the two tests is interleaved.

# The script is a bit messy, because two copies of [dune] cannot be
# invoked in parallel.

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ make -C test/HeightBalanced prepare
	@ make -C test/WeightBalanced prepare
	@ make -C test/HeightBalanced random_nodep & \
          make -C test/WeightBalanced random_nodep

.PHONY: bench
bench:
	@ make -C benchmark/BinarySearchTree test

.PHONY: install
install: all
	@ dune install -p $(THIS)

.PHONY: uninstall
uninstall:
	@ ocamlfind remove $(THIS) || true

.PHONY: reinstall
reinstall: uninstall
	@ make install

.PHONY: show
show: reinstall
	@ echo "#require \"bistro\";;\n#show Bistro;;" | ocaml

.PHONY: pin
pin:
	@ opam pin add $(THIS) . --yes

.PHONY: unpin
unpin:
	@ opam pin remove $(THIS) --yes

ASSEMBLY=$(shell find . -name "bistro__WeightBalanced.s")
.PHONY: assembly
assembly:
	@ dune clean && dune build --profile=release
	@ cat $(ASSEMBLY) \
	| grep -vw "\.loc" \
	| grep -vw "\.long" \
	| grep -vw "\.short" \
	| grep -vw "\.asciz" \
	| gsed "s/L[[:digit:]]\+/L/g" \
	> simplified.s
	@ open -a /Applications/Emacs.app/ simplified.s

# ------------------------------------------------------------------------------

# Documentation.

DOCDIR = _build/default/_doc/_html
DOC    = $(DOCDIR)/index.html

.PHONY: doc
doc:
	@ rm -rf _build/default/_doc
	@ dune clean
	@ dune build @doc
	@ echo "You can view the documentation by typing 'make view'".

.PHONY: view
view: doc
	@ echo Attempting to open $(DOC)...
	@ if command -v firefox > /dev/null ; then \
	  firefox $(DOC) ; \
	else \
	  open -a /Applications/Firefox.app/ $(DOC) ; \
	fi

.PHONY: export
export: doc
	ssh yquem.inria.fr rm -rf public_html/$(THIS)/doc
	scp -r $(DOCDIR) yquem.inria.fr:public_html/$(THIS)/doc

# ------------------------------------------------------------------------------

# [make versions] compiles the package under many versions of OCaml,
# whose list is specified below.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.14.2 \
  5.0.0 \
  5.1.0 \
  5.2.0

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions

.PHONY: handiwork
handiwork:
	@ for v in $(VERSIONS) ; do \
	    opam install --switch $$v core core_unix cppo monolith ocamlfind ; \
	  done

# ------------------------------------------------------------------------------

# [make headache] updates the headers.

HEADACHE := headache
HEADER   := header

.PHONY: headache
headache:
	@ for f in $(shell gfind . -type f -regex ".*\.mli?") ; do \
	  $(HEADACHE) -c headache.config -h $(HEADER) $$f ; \
	done

# -------------------------------------------------------------------------

# Publishing a release.

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make uninstall
	@ make clean
	@ make install
# Check the current package description.
	@ opam lint
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Upload. (This automatically makes a .tar.gz archive available on gitlab.)
	@ git push
	@ git push --tags
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  \"make publish\"   to publish a new opam package"
	@ echo "  \"make export\"    to upload the documentation to yquem.inria.fr"

.PHONY: publish
publish:
# Publish an opam description.
	@ opam publish -v $(DATE) $(THIS) $(ARCHIVE) .

.PHONY: undo
undo:
# Undo the last release (assuming it was done on the same date).
	@ git tag -d $(DATE)
	@ git push -u origin :$(DATE)
