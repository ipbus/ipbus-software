MAKE=make
PACKAGES = \
	extern/boost \
	extern/erlang \
	extern/pugixml \
	uhal/BoostSpiritGrammars \
	uhal/uhal \
	uhal/tests \
	controlhub

TARGETS=clean rpm build


.PHONY: $(TARGETS)
default: build

$(TARGETS):
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done