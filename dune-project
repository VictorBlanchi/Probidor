(lang dune 2.8)

(license MIT)

(authors "Victor Blanchi" "Mathis Bouverot-Dupuis")

(maintainers "victor.blanchi@ens.psl.eu" "mathis.bouverot@ens.psl.eu")

(package
 (name probidor)
 (synopsis
  "A game engine, a GUI and several (artificial) players for the game Quoridor")
 (depends
  (ocaml
   (>= 5.1.1))
  (ocamlformat
   (= 0.26.1))
  (ocaml-lsp-server
   (>= 1.17.0))
  (user-setup
   (>= 0.7))
  (lwt
   (>= 5.7.0))
  (lwt_ppx
   (>= 2.1.0))
  (yojson
   (>= 2.1.1))
  (ocamlgraph
   (>= 2.1.0))
  (qcheck-core
   (>= 0.21.3))
  (ppx_deriving
   (>= 5.2.1))))

(generate_opam_files)
