# Artifact for the paper "Revisiting Row Polymorphism for Set-Theoretic Types"

This artifact is a modified version of the SSTT library implementing polymorphic records.
The original SSTT library is released under MIT license.

## Running the web prototype

A WebAssembly version can be tested directly in the web browser.
This version is slower than the native version.

```
cd web
python3 -m http.server 8080
```

SSTT should then be accessible from your web browser: http://localhost:8080/

You can find examples in `REPL.md` and `src/tests/tests.txt`.


## Native version installation

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
```

The native version can then be built as follows:
```
make deps
make
```
