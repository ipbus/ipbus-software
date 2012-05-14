MAKE=make
PACKAGES = extern/boost 

TARGETS=clean rpm build


.PHONY: $(TARGETS)
default: build

$(TARGETS):
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done