(lang dune 3.14)

(name calyxium)

(generate_opam_files true)

(source 
 (github Calyxium-Lang/Calyxium))

(authors "Calyxium Foundation")
(maintainers "Codezz-ops")

(license MIT)

(package
 (name calyxium)
 (synopsis "An interpreted language")
 (description "Calyxium is an interpreted programming language written in OCaml.")
 (depends ocaml dune)
 (tags
  ("functional" "imperative" "object-oriented" "multi-paradigm" "interpreter" "language" "typed")))