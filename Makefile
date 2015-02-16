ifeq ($(Set), uhal_client)
PACKAGES = \
        cactuscore/extern/boost \
        cactuscore/extern/pugixml \
        cactuscore/uhal
else ifeq ($(Set), uhal)
PACKAGES = \
        cactuscore/extern/boost \
        cactuscore/extern/erlang \
        cactuscore/extern/pugixml \
        cactuscore/uhal \
        cactuscore/controlhub
else ifeq ($(Set), ts)
PACKAGES = \
        cactuscore/ts \
        cactuscore/candela \
        cactusprojects/central \
	cactusprojects/csctf \
        cactusprojects/dtsc \
        cactusprojects/dttf \
        cactusprojects/gct \
	cactusprojects/gt \
        cactusprojects/gmt \
        cactusprojects/l1page \
        cactusprojects/retri \
        cactusprojects/subsystem \
	cactusprojects/tcds \
        cactusprojects/ttc 
else ifeq ($(Set), tsdev)
PACKAGES = \
        cactuscore/ts \
        cactuscore/candela \
        cactusprojects/subsystem 
else ifeq ($(Set), tssub)
PACKAGES = \
        cactusprojects/central \
        cactusprojects/csctf \
        cactusprojects/dtsc \
        cactusprojects/dttf \
        cactusprojects/gct \
        cactusprojects/gt \
        cactusprojects/gmt \
        cactusprojects/l1page \
        cactusprojects/retri \
        cactusprojects/tcds \
        cactusprojects/ttc
#else ifeq ($(Set), tsupgrades)
#PACKAGES = \
#	cactusupgrades/projects/ugt \
#	cactusupgrades/projects/s1calol2
        
endif

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build 

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
