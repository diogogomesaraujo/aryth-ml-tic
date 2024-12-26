(lang dune 3.7)
(using menhir 2.1)

(name interpreter)

(generate_opam_files true)

(source
 (github diogogomesaraujo/interpreter))

(authors "Diogo Araújo")

(maintainers "Diogo Araújo")

(license MIT)

(package
 (name interpreter)
 (synopsis "Simple OCaml Interpreter")
 (description "A simple interpreter to parse math expressions")
 (depends ocaml dune)
 (tags
  (topics interpreter)))
