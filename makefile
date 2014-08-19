.PHONY: src

LINK            = ivi
EXECUTABLE      = src/ivi.bin
DIR_ON_PATH     = /usr/bin
INSTALL_PATH    = $(DIR_ON_PATH)/$(LINK)

all: build link

build: 
	$(MAKE) -C src

link:
	ln -sf $(EXECUTABLE) $(LINK)

install:
	$(MAKE) -C src install
	sudo ln -sf $(LINK) $(INSTALL_PATH)
	make all
