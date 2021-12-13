(lang dune 2.9)

(using menhir 2.1)

(generate_opam_files true)

(authors "Conner Delahanty" "Vinicius Lepca")

(maintainers "cdelaha1@jhu.edu" "vlepca1@jhu.edu")

(package
 (name broqli)
 (synopsis "An OCaml based DBMS for recursive queries")
 (description
  "BROQLI is an interpreter for BROQL, a no-SQL database system that allows users to make complex recursive queries.")
 (depends
  (ounit2
   (> 2.2.4))
  (core
   (> 0.14.1))
  (menhir
   (> 2.1))))
