#!/bin/sh

if ! test -d benchmarks
then
    echo "Script must be run from the project root"
    exit 1
fi
CFG=src/lib/sstt/core/utils/config.ml
BNC=src/bin/benchmark.ml
cp "$CFG" "$CFG".backup
cp "$BNC" "$BNC".backup

cleanup() {
    test -f "$CFG".backup && mv "$CFG".backup "$CFG"
    test -f "$BNC".backup && mv "$BNC".backup "$BNC"
}

abort () {
    cleanup
    echo "Interrupted"
    exit 1
}
trap abort INT
sed -i "$BNC" -e 's/let *raw_output *= .*/let raw_output = true/'
for c in benchmarks/config/*.pre
do
    echo "Running configuration $(basename $c)"
    output=`basename "$c" .pre`.log
    rm -f "$output"
    cp "$c" "$CFG"
    for b in benchmarks/[0-2]*.json
    do
	echo "   Input $(basename $b)"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = false/'
	dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep -v 'space\|errors' | cut -f 2 -d ':' >> "$output"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = true/'
	dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep 'space\|errors' | cut -f 2 -d ':' >> "$output"
    done
done
paste -d '&' benchmarks/00_prelude.log [0-9]*.log | sed -e 's/&/ & /g'  -e 's:$:\\\\:g' | tee benchmark.log
