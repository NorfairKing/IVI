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
	haddock -w --html -o ../doc Ivi.hs

clean:
	rm -f *.o *.hi
