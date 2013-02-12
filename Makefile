ifeq ($(Set), uhal)
PACKAGES = \
        extern/boost \
        extern/erlang \
        extern/pugixml \
        uhal \
        controlhub
else
PACKAGES = \
        extern/boost \
        extern/erlang \
        extern/pugixml \
        uhal \
        pycohal \
        controlhub \
	\
        cactuscore/extern/icons \
        cactuscore/extern/dojo \
        cactuscore/ts/toolbox \
        cactuscore/ts/ajaxell \
        cactuscore/ts/framework \
        cactuscore/ts/l1ce \
        cactuscore/ts/itf \
	cactuscore/ts/xdaqclient \
	\
	cactusprojects/subsystem/supervisor \
	cactusprojects/subsystem/worker \
	cactusprojects/subsystem/xdaq \
	\
        cactusprojects/central/cell \
	\
        cactusprojects/retri \
	\
        cactusprojects/ttc/ts/TTCUtils \
        cactusprojects/ttc/ts/TTCciCell \
        cactusprojects/ttc/ts/TTCmiCell \
        cactusprojects/ttc/ts/TTCMonitoringCell \
	\
        cactusprojects/l1page
endif

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build all 

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
