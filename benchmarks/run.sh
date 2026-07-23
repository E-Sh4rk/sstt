#!/bin/sh

if ! test -d benchmarks
then
    echo "Script must be run from the project root"
    exit 1
fi

trap ctrl_c INT

ctrl_c () {
    cp "$CFG".orig "$CFG"
    exit 1
}

CFG=src/lib/sstt/core/utils/config.ml
CORPUS_A=benchmarks/0_hm.json
CORPUS_B=benchmarks/1_union_inter.json
CORPUS_C=benchmarks/2_dyn.json
echo " \\GRAYLINE    & Building  
 \\GRAYLINE    & Solving   
 \\GRAYLINE A. & Total
 \\GRAYLINE    & Slowdown
 \\GRAYLINE    & \\# Sol.
 \\GRAYLINE  $(cat ${CORPUS_A} | grep '\"vars\"' | wc -l) & Size      
 \\GRAYLINE instances   & Avg. Size 
 \\GRAYLINE    & Peak Size 
 \\GRAYLINE    & Timeout   
              & Building  
              & Solving   
          B.  & Total
              & Slowdown
	      & \\# Sol.
         $(cat ${CORPUS_B} | grep '\"vars\"' | wc -l)     & Size      
         instances & Avg. Size 
              & Peak Size 
              & Timeout   
 \\GRAYLINE    & Building  
 \\GRAYLINE   & Solving   
 \\GRAYLINE C. & Total
 \\GRAYLINE    & Slowdown
 \\GRAYLINE   & \# Sol.
 \\GRAYLINE $(cat ${CORPUS_C} | grep '\"vars\"' | wc -l) & Size      
 \\GRAYLINE instances  & Avg. Size 
 \\GRAYLINE    & Peak Size 
 \\GRAYLINE    & Timeout   " > benchmarks/00_prelude.log


REF=output/01_bdt_opt_sub_opt_tall.ml.log
for c in benchmarks/config/*.pre
do
    echo "Running configuration $(basename $c)"
    output=output/`basename "$c" .pre`.log
    rm -f "$output"
    cp "$c" "$CFG"
    L=3
    for b in ${CORPUS_A} ${CORPUS_B} ${CORPUS_C}
    do
	echo "   Corpus $(basename $b): "
        echo -n "       Timing: "
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = false/'
        for i in 1 2 3
        do
            echo -n "$i/3 "
	    opam exec -- dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep -v 'space\|errors' | cut -f 2 -d ':' >> "$output"."$i"
        done
        echo
        paste "$output".* | while read l;
        do
           echo "2 k $l + + 3 / p" | dc | sed -e 's/[.]00/g/' >> "$output"
        done
        rm -f "$output".*
	SD="-"
	if [ "$output" != "$REF" ]
	then
	    ORIG=`cat "$REF" | sed -n "${L}p"`
	    CUR=`cat "$output" | sed -n "${L}p"`
	    SD="\\\\SD\{$(echo 2 k "$CUR" "$ORIG" '/' p | dc)\}"
	fi
	sed -i "${L}a\\
$SD" "$output"
        echo "       Memory: 1/1"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = true/'
	opam exec -- dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep 'space\|errors' | cut -f 2 -d ':' >> "$output"
	L=$(($L + 9))
    done
    cp "$CFG".orig "$CFG"
done
paste -d '&' benchmarks/00_prelude.log output/[0-9]*.log | sed -e 's/&/ & /g'  -e 's:$:\\\\:g' | tee output/benchmark.tex

