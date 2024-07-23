(lang dune 3.0)

(name dowsing)

(generate_opam_files true)

(source
 (github Drup/dowsing))

(authors "Gabriel Radanne")

(maintainers "Gabriel Radanne")

(license MIT)

(package
 (name dowsing-lib)
 (synopsis "Dowsing's index for search by type, usable on any database with a 'type' column")
 (depends
  (ocaml (>= "5.1")) dune
  fmt containers containers-data iter bos
  diet ocamlgraph zarith atomic ppx_deriving
  trace-tef
  (alcotest :with-test)
  (benchmark :with-test)
  (bechamel :with-test)
  (bechamel-notty :with-test)
  (bechamel-js :with-test)
))
 
(package
 (name dowsing)
 (synopsis "Dowsing is a type of divination employed in attempts to locate functions by giving a type")
 (depends
  (ocaml (>= "5.1")) dune
  cmdliner
  odoc odig 
  (dowsing-lib (= :version))
  (alcotest :with-test)
  (benchmark :with-test)
))
