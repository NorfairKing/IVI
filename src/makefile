.PHONY: Scripts

all: bootstrap bare documentation

GHC         = ghc
GHC_FLAGS   =   -Wall \
                --make

MAIN_SRC    = Ivi.hs
EXECUTABLE  = ivi.bin

bare:
	$(GHC) $(GHC_FLAGS) $(MAIN_SRC) -o $(EXECUTABLE)

HADDOCK         = haddock
HADDOCK_FLAGS   =   --html  \
                    --pretty-html\
                    --no-warnings\
                    --print-missing-docs\

DOC_DIR         = ../doc

documentation:
	$(HADDOCK) $(HADDOCK_FLAGS) -o $(DOC_DIR) $(MAIN_SRC)

install:
	$(MAKE) -C Scripts install

bootstrap:
	$(MAKE) -C Scripts

clean:
	rm -f *.o *.hi
