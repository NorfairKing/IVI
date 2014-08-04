.PHONY: src

all:
	make build
	make link

build:
	$(MAKE) -C src

link:
	ln -sf src/ivi.bin ivi

install:
	$(MAKE) -C src install
	sudo ln -sf ivi /usr/bin/ivi
	make all
