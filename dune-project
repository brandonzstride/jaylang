(lang dune 3.23)

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
  baby
  (cmdliner (>= 2.1))
  menhir
  moonpool
  mtime
  nel
  (ocaml (>= 5.5))
  (ppxlib (>= 0.38))
  ppx_deriving
  psq
  z3
  ; test
  (alcotest :with-test)
  (ppx_enumerate :with-test)
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  ; dev setup
  (landmarks :with-dev-setup)
  (landmarks-ppx :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)
  ))


