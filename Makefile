ifeq ($(Set), uhal)
PACKAGES = \
        cactuscore/extern/binutils \
        cactuscore/extern/boost \
        cactuscore/extern/erlang \
        cactuscore/extern/pugixml \
        cactuscore/uhal \
        cactuscore/controlhub
else ifeq ($(Set), ts)
PACKAGES = \
        cactuscore/ts \
	cactuscore/confdb \
	cactusprojects/subsystem \
	cactusprojects/retri \
	cactusprojects/central \
	cactusprojects/ttc \
	cactusprojects/l1page \
	cactusprojects/gt \
	cactusprojects/gmt \
	cactusprojects/dtsc
endif

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build 

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
