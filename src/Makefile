.PHONY: Scripts
all:
	make bootstrap
	make bare
	make documentation

bootstrap:
	$(MAKE) -C Scripts bootstrap

bare:
	ghc -Wall --make Ivi.hs -o ivi.bin

documentation:
	haddock --html -o ../doc Ivi.hs

verbose:
	ghc -v -Wall --make Ivi.hs -o ivi.bin
	make clean

clean:
	rm -f *.o *.hi
