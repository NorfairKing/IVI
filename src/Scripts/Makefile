.SILENT: bootstrap
bootstrap:
	echo "Getting ready to bootstrap IVI."	
	echo "Building the bootstrap from source."
	ghc -Wall --make Bootstrap.hs -o bootstrap.bin
	echo "Bootstrap program built, starting the bootstrap."
	echo
	./bootstrap.bin
	echo
	echo "Bootstrap done."
	make clean

clean:
	rm -f *.o *.hi
