PACKAGES = \
	log \
	grammars \
	uhal \
	tests \
	pycohal \
	gui \
	tools

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=all clean build cleanrpm rpm

.PHONY: $(TARGETS)
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
