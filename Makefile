MAKE=make
PACKAGES = extern/boost \
	extern/stlsoft \
	extern/pantheios

TARGETS=clean rpm build


.PHONY: $(TARGETS)
default: build

$(TARGETS):
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done