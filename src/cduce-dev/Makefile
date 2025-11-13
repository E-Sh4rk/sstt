.PHONY: all clean

# Directories
DUNE := dune
CDUCE_EXE := _build/default/driver/cduce.exe

all:
	$(DUNE) build
	chmod +w $(CDUCE_EXE)
	cp $(CDUCE_EXE) cduce

clean:
	rm -f *~ .*~ cduce
	$(DUNE) clean

install:
	$(DUNE) install

test: all
	dune runtest
