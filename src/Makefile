.PHONY: Scripts
all:
	make bootstrap
	make bare
	make documentation

bootstrap:
	$(MAKE) -C Scripts

bare:
	ghc -Wall --make Ivi.hs -o ivi.bin

documentation:
	haddock -w --html -o ../doc Ivi.hs

install:
	$(MAKE) -C Scripts install

clean:
	rm -f *.o *.hi
