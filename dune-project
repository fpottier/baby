(lang dune 3.11)
(name baby)
(generate_opam_files true)
(package
  (name baby)
  (authors
    "François Pottier"
  )
  (maintainers
    "François Pottier"
  )
  (source (uri git+https://github.com/fpottier/baby.git))
  (homepage https://github.com/fpottier/baby/)
  (bug_reports https://github.com/fpottier/baby/issues)
  (synopsis "Fast sets based on balanced binary search trees")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    (cppo (>= 1.7.0))
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
