(lang dune 3.11)
(generate_opam_files true)
(package
  (name bbst)
  (authors
    "François Pottier"
  )
  (maintainers
    "François Pottier"
  )
  (homepage https://github.com/fpottier/bbst/)
  (bug_reports francois.pottier@inria.fr)
  (synopsis "Several kinds of balanced binary search trees")
  (depends
    ocaml
    dune
    cppo
    (monolith :with-test)
  )
)
