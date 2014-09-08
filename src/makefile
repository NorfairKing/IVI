.PHONY: Scripts


GHC         = ghc
GHC_FLAGS   =   -Wall \
                --make

MAIN_SRC    = Ivi.hs
EXECUTABLE  = ivi.bin

ECHO        = echo -e


bare:
	$(ECHO)
	$(ECHO) "----------------"
	$(ECHO) "| Building IVI |"
	$(ECHO) "------------------------------------------------------------------"
	$(GHC) $(GHC_FLAGS) $(MAIN_SRC) -o $(EXECUTABLE)
	$(ECHO) "------------------------------------------------------------------"
	$(ECHO)

HADDOCK         = haddock
HADDOCK_FLAGS   =   --html  \
                    --pretty-html\
                    --no-warnings\
                    --print-missing-docs\

DOC_DIR         = ../doc

regular: bootstrap bare

all: bootstrap bare documentation

documentation:
	$(HADDOCK) $(HADDOCK_FLAGS) -o $(DOC_DIR) $(MAIN_SRC)

install:
	$(MAKE) -C Scripts install

bootstrap:
	$(MAKE) -C Scripts

clean:
	rm -f *.o *.hi
