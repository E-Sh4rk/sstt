#!/bin/sh

if ! test -d benchmarks
then
    echo "Script must be run from the project root"
    exit 1
fi
CFG=src/lib/sstt/core/utils/config.ml

for c in benchmarks/config/*.pre
do
    echo "Running configuration $(basename $c)"
    output=`basename "$c" .pre`.log
    rm -f "$output"
    cp "$CFG" "$CFG".backup
    cp "$c" "$CFG"
    for b in benchmarks/[0-2]*.json
    do
	echo "   Input $(basename $b)"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = false/'
	dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep -v 'space' | cut -f 2 -d ':' >> "$output"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = true/'
	dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep 'space' | cut -f 2 -d ':' >> "$output"
	echo >> "$output"
	echo >> "$output"
    done
    mv "$CFG".backup "$CFG"
done
cat benchmarks/00_prelude.log [0-9]*.log | paste -d '&' | sed -e 's/&/ & /g' | tee benchmark.log

