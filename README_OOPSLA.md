# Artifact for the paper "Revisiting Row Polymorphism for Set-Theoretic Types"

This directory contains the source code for the set-theoretic type library SSTT,
implementing the subtyping and tallying algorithms defined in Section 3 of the paper.
To build it and test the REPL locally, follow the section [native version](#native-version).
Alternatively, a Wasm version hosted by Github is available [here](https://e-sh4rk.github.io/sstt/).
Note that the Wasm version is significantly slower than the native one.

The syntax for the REPL is explained in `manual/REPL.md`  
Row-polymorphism specificities are explained in `manual/ROWPOLY.md`  
Source code documentation: https://e-sh4rk.github.io/sstt/doc/ (or `make doc` to generate)

Claims made in the paper:
- Example in Listing 3, Section 4.2:
```
> [ { ;; `R } <= { l1: int ; l2: bool ;; any? } ];;
[ `R: { l1 : `R ; l2 : `R ;; empty } ]
[ `R: { l1 : empty ;; `R } ]
[ `R: { l1 : `R & int ; l2 : `R & bool ;; `R } ]
[ `R: { l2 : empty ;; `R } ]
```

## Native version

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
```

You can then build and run the native version as follows:
```
make deps
make
```

## Web version

A WebAssembly version can be built and tested directly in the web browser.
This version is slower than the native version.

```
make web-deps
make wasm
cd web
python3 -m http.server 8080
```

SSTT should then be accessible from your web browser: http://localhost:8080/

A prebuilt Wasm version is available on Github: https://e-sh4rk.github.io/sstt/
