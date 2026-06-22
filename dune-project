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
  (cmdliner (>= 2.0))
  menhir
  moonpool
  mtime
  (ocaml (>= 5.5))
  preface
  psq
  vector
  z3
  ))


