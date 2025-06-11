(lang dune 3.14)
(using menhir 3.0)

(name calyxium)

(generate_opam_files true)

(source 
 (github Calyxium-Lang/Calyxium))

(authors "Calyxium Foundation")
(maintainers "Codezz-ops TheDevConnor Zombii Iris")

(license MIT)

(package
 (name calyxium)
 (synopsis "An interpretered Language")
 (description "Calyxium is an interpretered programming language written in OCaml.")
 (depends ocaml dune menhir ANSITerminal)
 (tags
  ("functional" "interpreter" "language")))