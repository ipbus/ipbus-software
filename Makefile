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
        cactuscore/extern/icons \
        cactuscore/extern/dojo \
        cactuscore/ts/toolbox \
        cactuscore/ts/ajaxell \
        cactuscore/ts/framework \
        cactuscore/ts/l1ce \
        cactuscore/ts/itf \
	cactuscore/ts/xdaqclient \
	\
	cactusprojects
endif

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build 

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
