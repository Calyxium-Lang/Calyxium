(lang dune 3.14)
(using menhir 3.0)

(name calyxium)
(version 0.1.0)

(generate_opam_files true)

(source
 (github calyxium-lang/calyxium))

(authors "Calyxium Foundation")
(maintainers "Codezz-ops")

(license MIT)

(package
 (name calyxium)
 (synopsis "Multi-paradigm, strongly typed programming language")
 (description "Calyxium is a multi-paradigm, monomorphic, strongly statically typed language supporting ad-hoc polymorphism, combining functional, imperative, and object-oriented programming styles.")
 (homepage "https://calyxium-lang.github.io")
 (documentation "https://calyxium-lang.github.io/docs")
 (depends ocaml dune menhir)
 (tags
  ("functional" "imperative" "object-oriented" "multi-paradigm" "interpreter" "language" "typed" "monomorphic" "static-typing" "ad-hoc-polymorphism")))