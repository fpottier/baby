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
  (source (uri git+https://github.com/fpottier/bistro.git))
  (homepage https://github.com/fpottier/bistro/)
  (bug_reports https://github.com/fpottier/bistro/issues)
  (synopsis "Fast sets based on balanced binary search trees")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    cppo
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
