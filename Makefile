
.NOTPARALLEL:


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


BUILD_HOME = $(shell pwd)
include config/Makefile.macros

ifneq ($(BUILD_BOOST), 1)
    PACKAGES := $(filter-out extern/boost, $(PACKAGES))
endif

ifneq ($(BUILD_PUGIXML), 1)
    PACKAGES := $(filter-out extern/pugixml, $(PACKAGES))
endif

ifneq ($(BUILD_ERLANG), 1)
    PACKAGES := $(filter-out extern/erlang, $(PACKAGES))
endif

$(info PACKAGES=$(PACKAGES))


VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=all clean build cleanrpm rpm uninstall install check-version

.PHONY: $(TARGETS) 
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)
