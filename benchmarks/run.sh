#!/bin/sh

if ! test -d benchmarks
then
    echo "Script must be run from the project root"
    exit 1
fi

trap ctrl_c INT

ctrl_c () {
    cp "$CFG".orig "$CFG"
}

CFG=src/lib/sstt/core/utils/config.ml
CORPUS_A=benchmarks/0_hm.json
CORPUS_B=benchmarks/1_union_inter.json
CORPUS_C=benchmarks/2_dyn.json
echo " \\GRAYLINE    & Building  
 \\GRAYLINE    & Solving   
 \\GRAYLINE A.   & Total     
 \\GRAYLINE    & \\# Sol.
 \\GRAYLINE  $(cat ${CORPUS_A} | grep '\"vars\"' | wc -l) & Size      
 \\GRAYLINE instances   & Avg. Size 
 \\GRAYLINE    & Peak Size 
 \\GRAYLINE    & Timeout   
              & Building  
              & Solving   
          B.  & Total
	      & \\# Sol.
         $(cat ${CORPUS_B} | grep '\"vars\"' | wc -l)     & Size      
         instances & Avg. Size 
              & Peak Size 
              & Timeout   
 \\GRAYLINE    & Building  
 \\GRAYLINE   & Solving   
 \\GRAYLINE C. & Total
 \\GRAYLINE   & \# Sol.
 \\GRAYLINE $(cat ${CORPUS_C} | grep '\"vars\"' | wc -l) & Size      
 \\GRAYLINE instances  & Avg. Size 
 \\GRAYLINE    & Peak Size 
 \\GRAYLINE    & Timeout   " > benchmarks/00_prelude.log



for c in benchmarks/config/*.pre
do
    echo "Running configuration $(basename $c)"
    output=`basename "$c" .pre`.log
    rm -f "$output"
    cp "$c" "$CFG"
    for b in ${CORPUS_A} ${CORPUS_B} ${CORPUS_C}
    do
	echo "   Input $(basename $b)"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = false/'
	opam exec -- dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep -v 'space\|errors' | cut -f 2 -d ':' >> "$output"
	sed -i "$CFG" -e 's/let *benchmark_size *= .*/let benchmark_size = true/'
	opam exec -- dune exec --display=quiet -- src/bin/benchmark.exe "$b" | grep 'space\|errors' | cut -f 2 -d ':' >> "$output"
    done
    cp "$CFG".orig "$CFG"
done
paste -d '&' benchmarks/00_prelude.log [0-9]*.log | sed -e 's/&/ & /g'  -e 's:$:\\\\:g' | tee benchmark.log

