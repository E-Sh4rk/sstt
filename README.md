# Artifact for the paper "Implementing Set-Theoretic Types"

## Running the web version of the REPL

A WebAssembly version of the REPL can be tested directly in the web browser.
This version is slower than the native version. The http server can be started this way:

```
cd web
python3 -m http.server 8080
```

SSTT should then be accessible from your web browser: http://localhost:8080/

You can find examples in `REPL.md` and `src/tests/tests.txt`.

The benchmarks **cannot** be run on the WebAssembly version.

## API Documentation

The API Documentation can be found in `web/doc/`, or by visiting
http://localhost:8080/doc/ after starting the http server.

## Replicating the benchmarks

Benchmarks must be run on the native version.

### Installation

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
```

Dependencies of this project can then be installed using `make deps` (this will also install the CDuce package, which is required for the benchmarks).

### Running the benchmarks

The directoy `benchmarks` contains the benchmark files that are processed when running `make benchmark`.
To run individual files one can do:
```
dune exec -- src/bin/benchmark.exe  benchmarks/0_hm.json
```

The benchmarks can be configured by setting the corresponding parameters in `src/lib/sstt/core/utils/config.ml`.

The different variants tested in the paper can be found in benchmarks/config/*.ml.pre (each file corresponds
to one column of the table). To generate the full table as provided in the paper, one can run:
```
benchmarks/run.sh
```
This will recompile sstt for each variant and perform two runs for each combination of input file and
configuration (one run to measure speed and one to measure the size of types). The output is
printed in the terminal and saved in `*.log` files in the project root.

Benchmark files can be generated from the type-checker [MLsem](https://github.com/E-Sh4rk/MLsem),
by running `make record`. This will generate JSON files in the `tests` directory (one for each test file).
