.PHONY: src
build:
	$(MAKE) -C src
	ln -sf src/ivi.bin ivi

install:
	sudo ln -sf ivi /usr/bin/ivi
