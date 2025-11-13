# Testing

## Layout
Testing is split in several categories. Each category contains a `good/` and
optionally a `bad` directory for expected to succeed and expected to fail tests. Tests use dune's cram test format. Each directory should contain a dune file of the form:
```
(cram
 (package <p>)
 (applies_to :whole_subtree)
 (deps
  (source_tree ../common/)
  <other deps>))
```
where `<p>` is the package to which the test belongs (`cduce-types`, `cduce` or `cduce-tools`) name and `<other deps>` are the dependencies the tests require besides the files in `common`. This can be used to rely on an auxiliary program used to run the tests in that particular directory.
In each `good` or `bad` sub-directory there is a directory named `foo.t` that contains the test.
Inside it is a file `run.t` that contains the cram test commands and expected output. The other files are auxiliary files needed to run the test.
The directories are:

- `full`: 
    - `good`: compiles a CDuce program, runs it and diffs its output against an expected output. The
    compilation must succeed, the execution must succeed, and the output must be identical to the
    expected one.
    - `bad`: compiles a CDuce program, runs it and diffs its output against an
    expected output. The compilation must succeed, but the execution must fail
    with a status code and an error message which is checked against the expected file.

- `ocaml_ext`:
    - `good`: Like `full/good` but uses OCaml primitives embeded in the runtime
    - `bad`: Like `full/bad` but uses OCaml primitives embeded in the runtime

- `types`:
  - `good`: unit tests using the `Types` API: set operations, subtyping, tallying.

- `type_printer`:
    - `good` contains tests using the auxiliary `bin/type_printer.exe` program. This program takes as input a CDuce file consisting only of type definitions. The test programs pretty-prints then reparses its outputs and checks that the type obtained is semantically equivalent to the original.

- `sandbox`:
    This special directory contains a `sandbox.exe` binary that just compiles and print stuff to stdout. It can be used to quickly test a CDuce API function. A toplevel dune alias `@sandbox` is created for convenience.
    The command `dune build @sandbox` will compile and execute this program.

## Running tests
Tests are simply run by invoking
```
dune runtest
```

## Changing the result of a test
Expected results of tests are in `run.t` files. When the result of an *existing*
test changes (and thus the diff fails), one needs to
promote the new result as the reference one, using
```
dune promote
```
right after the failing ```dune runtest```.