.PHONY: Scripts


GHC         = ghc
GHC_FLAGS   =   -Wall \
                --make

HERE        = $(shell pwd)
MAIN_SRC    = $(HERE)/Ivi.hs
EXECUTABLE  = $(HERE)/ivi.bin

HADDOCK         = haddock
HADDOCK_FLAGS   =   --html  \
                    --pretty-html\
                    --no-warnings\
                    --print-missing-docs\

DOC_DIR         = ../doc

ECHO        = echo -e

regular: bootstrap prebuild build

all: bootstrap prebuild build documentation

bootstrap:
	$(MAKE) -C Scripts bootstrap

prebuild:
	$(MAKE) -C Scripts prebuild

build:
	$(ECHO)
	$(ECHO) "----------------"
	$(ECHO) "| Building IVI |"
	$(ECHO) "------------------------------------------------------------------"
	$(GHC) $(GHC_FLAGS) $(MAIN_SRC) -o $(EXECUTABLE)
	$(ECHO) "------------------------------------------------------------------"
	$(ECHO)

documentation:
	$(HADDOCK) $(HADDOCK_FLAGS) -o $(DOC_DIR) $(MAIN_SRC)

install:
	$(MAKE) -C Scripts install

clean:
	rm -f *.o *.hi *.dyn_o *.dyn_hi $(EXECUTABLE)
