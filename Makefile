MAKE=make
PACKAGES = 	extern/boost \
		extern/stlsoft \
		extern/pantheios \
		extern/pugixml \
		uhal/BoostSpiritGrammars \
		uhal/uhal \
		uhal/tests

TARGETS=clean rpm build


.PHONY: $(TARGETS)
default: build

$(TARGETS):
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done