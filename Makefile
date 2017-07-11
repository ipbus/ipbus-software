
Set?=all

ifeq ($(Set), uhal)
    PACKAGES = \
        extern/boost \
        extern/pugixml \
        uhal
else ifeq ($(Set), controlhub)
    PACKAGES = \
        extern/erlang \
        controlhub
else ifeq ($(Set), all)
    PACKAGES = \
        extern/boost \
        extern/erlang \
        extern/pugixml \
        uhal \
        controlhub
else
    $(error Invalid value for Set variable!)
endif

VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=clean rpm build 

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
