.SILENT: all bootstrap clean
all:
	echo "Getting ready to bootstrap IVI."	
	echo "Building the bootstrap from source."
	make Bootstrap
	echo "Bootstrap program built, starting the bootstrap."
	echo
	./bootstrap.bin
	echo
	echo "Bootstrap done."

bootstrap:
	$(MAKE) -C Bootstrap

clean:
	$(MAKE) -C Bootstrap clean
	rm -f *.o *.hi

.Phony: Bootstrap
