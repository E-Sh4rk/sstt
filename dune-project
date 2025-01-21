(lang dune 3.13)
(using menhir 3.0)

(name sstt)

(version 0.1.0)

(generate_opam_files true)

(authors "Mickael Laurent")

(maintainers "Mickael Laurent")

(homepage "https://github.com/E-Sh4rk/sstt")

(bug_reports "https://github.com/E-Sh4rk/sstt/issues")

(license "MIT")

(package
 (name sstt)
 (synopsis "Simple Set-Theoretic Types library")
 (depends (ocaml (>= 5.3.0)) dune zarith)
)

(package
 (name sstt-repl)
 (synopsis "Simple Set-Theoretic Types REPL library")
 (depends sstt ppx_expect menhir)
)

(package
 (name sstt-bin)
 (synopsis "Simple Set-Theoretic Types executable")
 (depends sstt-repl)
)
