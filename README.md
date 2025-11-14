# Simple Set-Theoretic Types (SSTT) library

## Instructions for the reviewers of PLDI

This directory contains an instrumented version of the SSTT library that
facilitates the activation/deactivation of some optimizations and the measure of performance.
It also embeds the benchmark files and corpuses used in the paper, as well as
the [CDuce implementation](https://gitlab.math.univ-paris-diderot.fr/cduce/cduce)
that can be used as a back-end for some benchmarks.

- BDTs are defined in `src/lib/sstt/core/utils/bdd.ml`
- Components are defined in `src/lib/sstt/core/components/`
- Nodes and descirptors are defined in `src/lib/sstt/core/`
- Subtyping is defined in a modular way, in each component (function `is_empty`)
- Tallying is defined in `src/lib/sstt/types/tallying.ml`
- Extensions (encodings) are defined in `src/lib/sstt/types/extensions/`
- Pretty-printing is defined in `src/lib/sstt/types/printer/`

### Testing the prebuilt REPL (Wasm)

SSTT features a REPL that can be tested directly in the web browser.

```
cd web
python3 -m http.server 8080
```

The SSTT REPL should then be accessible from your web browser: http://localhost:8080/  
Examples can be found in `REPL.md` or `src/tests/tests.txt`.

Note: this Wasm version is slower than the native version.

### Running the benchmarks

The Wasm build does not include the benchmarks.
To run the benchmarks, the [OCaml Package Manager](https://opam.ocaml.org/) must be installed first.

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
make deps
make benchmark
```

Some remarks:
- The benchmark files can be found in `benchmarks/`.
- The corpuses that have been used to generate the benchmark files can be found in `corpuses/`.
- To run the benchmarks on the CDuce backend instead of SSTT, you can modify `src/bin/benchmark.ml`
(definition `use_cduce_backend`)
- The configuration of SSTT (hash-consing, semantic simplification, etc.) can be modified in `src/lib/sstt/core/utils/config.ml`

## General instructions and documentation

SSTT is an OCaml library for manipulating set-theoretic types.  
**Disclaimer: this library is a work in progress and is subject to breaking change.**

Currently, it supports the following type constructors:
- Enums
- Tags
- Integer intervals
- Arrows
- Tuples of any arity
- Records
- Type variables

The following operations are implemented:
- Semantic subtyping
- DNF extraction and simplification
- Projections, application, record merging
- Substitution
- Tallying (= unification but with subtyping constraints)
- Pretty printing (or more generally, extraction of an algebraic representation)

It also provides a REPL that allows performing common operations (subtyping, tallying, etc.) with a conveninent syntax.
See [`REPL.md`](REPL.md) for examples and a description of the syntax.

## Installation

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
```

### Using OPAM

The easiest way to install this library is through [opam](https://opam.ocaml.org/), the OCaml Package Manager.  
The SSTT library can be installed as follows:

```
opam pin sstt ANONYMIZED_URL
```

The REPL binary `sstt` can also be installed this way:

```
opam pin sstt-repl ANONYMIZED_URL
opam pin sstt-bin ANONYMIZED_URL
```

## License

This software is distributed under the MIT license.
See [`LICENSE`](LICENSE) for more info.  
