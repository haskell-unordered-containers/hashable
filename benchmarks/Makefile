package := hashable
version := $(shell awk '/^Version:/{print $$2}' ../$(package).cabal)
lib := ../dist/build/libHS$(package)-$(version).a
ghc := ghc
ghc-flags := -Wall -O -hide-all-packages \
	-package-conf ../dist/package.conf.inplace -package base \
	-package hashable -package criterion \
	-package deepseq -package ghc-prim

%.o: %.hs
	$(ghc) $(ghc-flags) -c -o $@ $<

programs := bench

.PHONY: all
all: $(programs)

bench: $(lib) Benchmarks.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
