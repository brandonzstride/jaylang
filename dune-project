(lang dune 3.19)

(name jay)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github JHU-PL-Lab/jaylang))

(license Apache-2.0)

(authors "JHU PL Lab <pl.cs@jhu.edu>")

(maintainers "JHU PL Lab <pl.cs@jhu.edu>")

(package
 (name jay)
 (description "Semantic typing via program translation and symbolic execution")
 (depends
  (alcotest :with-test)
  baby
  (cmdliner (>= 2.1))
  menhir
  moonpool
  mtime
  nel
  (ocaml (>= 5.5))
  (ppxlib (>= 0.38))
  (ppx_enumerate :with-test)
  (ppx_sexp_conv :with-test)
  ppx_deriving
  psq
  (sexplib :with-test)
  z3
  ))


