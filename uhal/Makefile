MAKE=make
PACKAGES = \
	log \
	grammars \
	uhal \
	tests 

TARGETS=clean rpm build all


.PHONY: $(TARGETS)
default: build

$(TARGETS):
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done