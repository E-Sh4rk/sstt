split:
	ocaml unix.cma bench.ml split 0,10,50,100,500,1000 \
	split*.cd split*.xsl \
	| tee split.results

addrbook:
	ocaml unix.cma bench.ml addrbook 0,1000,5000,10000,50000,100000 \
	addrbook*.cd addrbook*.xsl \
	| tee addrbook.results
clean:
	rm -f split.*.xml addrbook.*.xml
	rm -f *~
	rm -f *.results
