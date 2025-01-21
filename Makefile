export COMMIT = $(shell git describe --always --tags HEAD)

all: build run

deps:
	opam install . --deps-only

test:
	opam exec -- dune runtest

doc:
	opam exec -- dune build @doc
	rm -rf web/doc
	cp -r _build/default/_doc/_html/ web/doc

promote:
	opam exec -- dune runtest --auto-promote

build:
	opam exec -- dune build src/native.exe

run:
	opam exec -- dune exec ./src/native.exe

js:
	opam exec -- dune build --profile release src/js.bc.js
	cp _build/default/src/js.bc.js web/sstt.js
	chmod +w web/sstt.js
	git describe --always --tags HEAD > web/version.txt
	chmod +w web/version.txt

web-deps:
	opam install zarith_stubs_js js_of_ocaml js_of_ocaml-ppx
	cd web ; npm ci

clean:
	opam exec -- dune clean
