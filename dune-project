(lang dune 3.0)

(name dowsing)

(generate_opam_files true)

(source
 (github Drup/dowsing))

(authors "Gabriel Radanne")

(maintainers "Gabriel Radanne")

(license MIT)

(package
 (name dowsing)
 (synopsis "Dowsing is a type of divination employed in attempts to locate functions by giving a type")
 (depends
   (ocaml (>= "5.1"))  dune
   fmt containers containers-data iter bos
   diet ocamlgraph zarith atomic ppx_deriving
   trace-tef
   odoc odig 
  (alcotest :with-test)
  (benchmark :with-test)
))
