# Simple Set-Theoretic Types (SSTT) library

SSTT is an OCaml library for manipulating set-theoretic types ([documentation](https://e-sh4rk.github.io/sstt/doc/)).  
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

It also provides a REPL ([web version](https://e-sh4rk.github.io/sstt/)) that allows performing common operations (subtyping, tallying, etc.) with a conveninent syntax.
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
opam pin sstt https://github.com/E-Sh4rk/sstt.git#main
```

The REPL binary `sstt` can also be installed this way:

```
opam pin sstt-repl https://github.com/E-Sh4rk/sstt.git#main
opam pin sstt-bin https://github.com/E-Sh4rk/sstt.git#main
```

## License

This software is distributed under the MIT license.
See [`LICENSE`](LICENSE) for more info.  
*This work is funded by the ERC CZ LL2325 grant.*
