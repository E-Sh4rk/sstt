# Installation Notes for CDuce

CDuce is written in the OCaml programming language. It has been
successfully compiled under Linux, macOS, 
and Windows 10
(See INSTALL.WIN32.md for installation under Windows). Any Unix system supporting the `opam`
package manager should work to.

## Prerequisites

An easy way to get a system ready to compile CDuce is to use the `opam` package manager.

It includes all the mandatory and optional packages listed below, and also the special modules needed to build the OCaml/CDuce interface. If running on Linux, the packages may also be available from your distribution.

## Dependencies

The CDuce distribution consists of four packages :
- `cduce-types`, a library implementing set-theoretic types. Its dependencies are:
  * `ocaml  >= 4.08.1` : the OCaml compiler
  * `dune   >= 2.8.0`  : the dune build system for OCaml
  * `zarith >= 1.12` : the OCaml library for manipulating big integers
  * `odoc   >= 1.5.0`  : the OCaml documentation generator.

- `cduce`, the compiler and runtime for the CDuce language. Its mandatory dependencies are:
  * `cduce-types` of the same version, see above.
  * `menhir` and `menhirLib >= 20181026` : the powerful LR(1) parser generator and runtime library
  * `sedlex >= 2.0` : a UTF-8 aware lexer generator.

  Some optional features require the following dependencies:
  
  * XML parsing requires either:
    - `ocaml-expat >= 1.1.0`, `pxp >= 1.2.9` or `markup >= 1.0.0-1`
  * HTML 4 parsing requires either:
    `pxp >= 1.2.9` or `markup >= 1.0.0-1`
  * HTML 5 parsing requires `markup >= 1.0.0-1`
  CDuce supports these three back-ends at the same time and allows one to select/disable them at runtime. The `ocaml-expat` package depends on
  the `libexpat` C library.

  * Loading of remote resources (HTML or XML files given by a URL, DTDs, …) require either :
    - `ocamlnet >= 4.1.8` or `ocurl >= 0.9.2`
  The `ocurl` package depends on the `libcurl` C library.

  * The OCaml/CDuce interface depends on :
    - `ocaml-compiler-libs >= 0.9.0`
  If present, the CDuce compiler will be built with support for loading OCaml code. Selected modules from OCaml's standard library are availble in the default toplevel. Note that recent version of `sedlex >= 2.4` add an indirect dependecy on `ocaml-compiler-libs` so the OCaml support will be built in, but can be disabled at runtime.

- `cduce-js`, a special version of the CDuce runtime and compiler that can be linked with `js_of_ocaml` code. It does not depend on external C libraries and implements XML/HTML parsing and URL loading via
the JavaScript browser API. The mandatory dependencies are:
    * `js_of_ocaml-compiler` and `js_of_ocaml-ppx >= 3.7.1`.

- `cduce-tools`, the utilities `dtd2cduce` and `cduce_mktop`. The mandatory dependencies are :
  * `cduce` of the same version
  * `ocamlfind >= 1.9.1` for the `cduce_mktop` utility
  * `pxp >= 1.2.9` for `dtd2cduce` (for DTD parsing).

## Installing via `opam`

With `opam` you can simply *pin* the directory containing CDuce sources:
```
  opam pin add -n cduce-types SOURCE
  opam pin add -n cduce SOURCE
  opam pin add -n cduce-js SOURCE
  opam pin add -n cduce-tools SOURCE
  opam install cduce cduce-tools
```
where `SOURCE` is the place where the CDuce sources are located. It can be a directory on your local filesystem or directly the URL of the git repository:
> https://gitlab.math.univ-paris-diderot.fr/cduce/cduce

or

> https://gitlab.math.univ-paris-diderot.fr/cduce/cduce#dev

to select the `dev` branch. The `opam` package manager
will take care of installing the mandatory dependencies.
If you install an *optional* dependency, `opam` will
take care of the recompilation of the `cduce` package.

## Manual compilation

Before compiling CDuce, you need to install the mandatory packages listed in the [Dependencies](#dependencies) section above.
 A convenience script
is given in the `tools` directory of CDuce sources :
```
tools/init_opam_switch.sh --install
```
which installs all the dependencies (mandatory and optional) with `opam` needed to compile the four packages. By default, the script let `opam` chose which version of the packages to install. The flag `--min-version` can be given to the script to force the installation of the minimal version of each package. This may downgrade some packages that are already installed. To see which packages get installed, you can use the `--print-deps` flag.

### Full build

If all dependencies (mandatory and optional) are installed, one can just build everything with:
```
dune build
```
If you are only interested in the `cduce.exe` binary, you can build it with
```
dune build driver/cduce.exe
```

### Partial build

The absence of optional dependencies of the `cduce` package do not prevent it to be built (their code is replaced by stubs and their functionality is unavailable).

If mandatory dependencies of `cduce-js` (`js_of_ocaml`) and `cduce-tools` (`findlib`) are not installed, one can perform a partial build as such:
```
dune build -p cduce-types,cduce
```

## Installation

(For Windows refer to the `INSTALL.WIN32.md` file).

Once built, you can install CDuce with:
```
dune install
```
or
```
dune install -p PACKAGES
```
where `PACKAGE` is a comma-separated list  of `cduce-types`, `cduce` `cduce-js` or `cduce-tools`. You can uninstall CDuce with:
```
dune uninstall
```
If `dune` is installed via `opam` and you wish to install CDuce in a root owned directory (e.g. `/usr/local`) you can do :
```
sudo $(which dune) install --prefix=/opt
```
this require that the `ocamlc` binary to be present in the *secure path* of `sudo`, since `dune` will query (but not use because of the `--prefix` option) the install path of `ocamlc`.
