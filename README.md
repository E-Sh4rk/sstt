# Simple Set-Theoretic Types (SSTT) library

SSTT is an OCaml library for manipulating set-theoretic types.  
[Documentation](https://e-sh4rk.github.io/sstt/doc/) - [REPL](https://e-sh4rk.github.io/sstt/) - [Manual](manual/REPL.md)

> [!NOTE]
> This library only implements a set-theoretic type algebra, it is **not** a full type system. If you are looking for a set-theoretic type system implementation, you can take a look at [MLsem](https://github.com/E-Sh4rk/MLsem).

> [!IMPORTANT]
> This library is a research artifact and is subject to breaking changes.

Currently, it supports the following built-in type constructors:
- Enums
- Integer intervals
- Arrows
- Tuples of any arity
- Records
- Tags (a.k.a. ``[Opaque Data Types](https://doi.org/10.1145/3798220)'')
- Type variables and row variables
- Set-theoretic connectives and equirecursive types

> [!TIP]
> The type algebra can be extended with other type constructors (e.g. Booleans, Strings, Lists, etc.) using encodings. For instance, [RSTT](https://github.com/E-Sh4rk/rstt) provides extensions for the R language.

It features the following operations on set-theoretic types:
- Semantic subtyping
- DNF extraction and simplification
- Usual type operators such as projections and arrow type applications
- Application of a substitution
- Tallying (= unification but with subtyping constraints)
- Pretty printing (or more generally, extraction of an algebraic representation)

> [!WARNING]
> This library is not thread-safe: it must be used from a single thread of a single domain.

## Installation

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
```

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
*This work is funded by the ERC CZ LL2325 grant and Université Paris-Saclay.*