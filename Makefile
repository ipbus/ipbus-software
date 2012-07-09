PACKAGES = \
	extern/boost \
	extern/erlang \
	extern/pugixml \
	uhal/log \
	uhal/grammars \
	uhal/uhal \
	uhal/tests \
	controlhub

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build all

.PHONY: $(TARGETS)
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
        ${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
