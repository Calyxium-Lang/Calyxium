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
 (synopsis "An interpretered Language")
 (description "Calyxium is an interpretered programming language written in OCaml.")
 (depends ocaml dune)
 (tags
  ("functional" "imperative" "object-oriented" "multi-paradigm" "interpreter" "language" "typed")))