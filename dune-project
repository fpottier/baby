(lang dune 3.11)
(generate_opam_files true)
(package
  (name bistro)
  (authors
    "François Pottier"
  )
  (maintainers
    "François Pottier"
  )
  (homepage https://github.com/fpottier/bistro/)
  (bug_reports francois.pottier@inria.fr)
  (synopsis "Several kinds of balanced binary search trees")
  (depends
    (ocaml (>= 4.14))
    cppo
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
