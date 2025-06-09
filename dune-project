(lang dune 3.14)
(using menhir 3.0)

(name Calyxium)

(generate_opam_files true)

(source 
 (github Calyxium-Lang/Calyxium))

(authors "Calyxium Foundation")
(maintainers "Codezz-ops TheDevConnor Zombii Iris")

(license MIT)

(package
 (name Calyxium)
 (synopsis "An interpretered Language")
 (description "Calyxium is an interpretered programming language written in OCaml.")
 (depends ocaml dune menhir zarith)
 (tags
  ("functional" "interpreter" "language")))