ifeq ($(Set), uhal_client)
PACKAGES = \
        extern/boost \
        extern/pugixml \
        uhal
else ifeq ($(Set), uhal)
PACKAGES = \
        extern/boost \
        extern/erlang \
        extern/pugixml \
        uhal \
        controlhub
        
endif

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build 

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
