.PHONY: $(BOOTSTRAP)

REPOSITORY          = git@github.com:NorfairKing/IVIBootstrap.git

BOOTSTRAP           = IVIBootstrap

EXECUTABLE_NAME     = bootstrap.bin
EXECUTABLE_DIR      = .
EXECUTABLE          = $(EXECUTABLE_DIR)/$(EXECUTABLE_NAME)

PREBUILD_SCRIPT		= prebuild.sh

NO_COLOR            = \x1b[0m
OK_COLOR            = \x1b[32;01m
ERROR_COLOR         = \x1b[31;01m
WARN_COLOR          = \x1b[33;01m

OK_STRING           = $(OK_COLOR)[OK]$(NO_COLOR)
ERROR_STRING        = $(ERROR_COLOR)[ERRORS]$(NO_COLOR)
WARN_STRING         = $(WARN_COLOR)[WARNINGS]$(NO_COLOR)

ECHO                = echo -e

bootstrap:
	$(ECHO)
	$(ECHO) "--------------------------"
	$(ECHO) "| Building the bootstrap |"
	$(ECHO) "------------------------------------------------------------------"
	make build_bootstrap
	$(ECHO) -n "Bootstrap built"
	$(ECHO) -e " $(OK_STRING)"
	$(ECHO) "------------------------------------------------------------------"
	$(ECHO)
	$(ECHO) "---------------------"
	$(ECHO) "| Bootstrapping IVI |"
	$(ECHO) "------------------------------------------------------------------"
	$(EXECUTABLE)
	$(ECHO) -n "Bootstrap done."
	$(ECHO) -e " $(OK_STRING)"
	$(ECHO) "------------------------------------------------------------------"
	$(ECHO)
    

build_bootstrap:
	$(MAKE) -C $(BOOTSTRAP)

prebuild:
	$(ECHO)
	$(ECHO) "---------------------------------"
	$(ECHO) "| Running Script prebuild hooks |"
	$(ECHO) "------------------------------------------------------------------"
	bash $(PREBUILD_SCRIPT)
	$(ECHO) "------------------------------------------------------------------"
	$(ECHO)

install:
	rm -rf $(BOOTSTRAP)
	git clone -q $(REPOSITORY)
	$(MAKE) -C $(BOOTSTRAP) install
	make bootstrap
	$(EXECUTABLE_DIR)/$(EXECUTABLE_NAME)

clean:
	$(MAKE) -C $(BOOTSTRAP) clean
	rm -f *.o *.hi *.dyn_o *.dyn_hi

