.PHONY: src
.SILENT: regular

LINK            = ivi
EXECUTABLE      = src/ivi.bin
DIR_ON_PATH     = /usr/bin
INSTALL_PATH    = $(DIR_ON_PATH)/$(LINK)

M               = $(MAKE) --no-print-directory --silent --directory

regular: build link

all: regular
	$M src all

build: 
	@$M src regular

link:
	@ln -sf $(EXECUTABLE) $(LINK)

install:
	cabal install --only-dependencies
	$M src install
	sudo ln -sf $(LINK) $(INSTALL_PATH)
	make all

clean:
	unlink $(LINK)
	@$M src clean
