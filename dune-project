(lang dune 3.11)
(name baby)
(generate_opam_files true)
(package
  (name baby)
  (authors
    "François Pottier <francois.pottier@inria.fr>"
  )
  (maintainers
    "François Pottier <francois.pottier@inria.fr>"
  )
  (source (uri git+https://github.com/fpottier/baby.git))
  (homepage https://github.com/fpottier/baby/)
  (bug_reports https://github.com/fpottier/baby/issues)
  (synopsis "Fast sets and maps based on balanced binary search trees")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    (cppo (>= 1.8.0))
    (monolith (>= 20241126))
  )
)
;; monolith is needed only for testing, but I do not know
;; how to say both :with-test and (>= 20241126).
;; dune accepts either indication,
;; but not both indications together.
