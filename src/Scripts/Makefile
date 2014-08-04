.SILENT: all bootstrap clean
all:
	echo "Getting ready to bootstrap IVI."	
	echo "Building the bootstrap from source."
	make bootstrap
	echo "Bootstrap program built, starting the bootstrap."
	echo
	./bootstrap.bin
	echo
	echo "Bootstrap done."

bootstrap:
	$(MAKE) -C IVIBootstrap

install:
	rm -rf IVIBootstrap
	git clone -q git@github.com:NorfairKing/IVIBootstrap.git
	$(MAKE) -C IVIBootstrap install
	make bootstrap
	./bootstrap.bin

clean:
	$(MAKE) -C IVIBootstrap clean
	rm -f *.o *.hi

.Phony: Bootstrap
