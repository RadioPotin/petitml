(lang dune 2.8)

(name petitml)

(authors "Dario Pinto <laradiopotin@gmail.com>")

(maintainers "Dario Pinto <laradiopotin@gmail.com>")

(source
  (github RadioPotin/petitml))

(generate_opam_files true)

(package
  (name petitml)
  (synopsis "Small ML implementation")
  (description "")
  (depends
    (ocaml (>= 4.12))
    (dune (>= 2.8))
  (bisect_ppx
    (and
      :with-test
      (>= 2.6)
      :dev))))
