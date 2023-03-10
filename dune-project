(lang dune 3.0)

(name dowsing)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
  (name        dowsing)
  (synopsis    "A short synopsis")
  (description "A longer description")
  (depends ocaml dune
    (containers      (>= 3.11))
    (containers-data (>= 3.11))
    (iter            (>= 1.6))
    (ocp-index       (>= 1.3.4)))
  (tags (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
