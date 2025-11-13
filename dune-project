(lang dune 3.17)
(using menhir 3.0)
(using mdx 0.4)

(name sstt)

(version 0.1.0)

(generate_opam_files true)

(authors
 "Anonymous"
 "Anonymous"
)

(maintainers
 "Anonymous"
 "Anonymous"
)

(license "MIT")

(package
 (name sstt)
 (synopsis "Simple Set-Theoretic Types library")
 (depends (ocaml (>= 5.3.0)) (mdx :with-test) dune zarith)
)

(package
 (name sstt-repl)
 (synopsis "Simple Set-Theoretic Types REPL library")
 (depends sstt menhir (cduce-types (= dev)) (cduce  (= dev)))
)

(package
 (name sstt-bin)
 (synopsis "Simple Set-Theoretic Types executable")
 (depends sstt-repl yojson)
)
