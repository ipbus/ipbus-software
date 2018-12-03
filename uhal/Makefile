
.NOTPARALLEL:


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

TARGETS=all clean build cleanrpm rpm install

.PHONY: $(TARGETS)
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)


# Uninstall target: Defined here, rather than in Makefiles for each individual uhal package (e.g. log, grammars, etc),
#     because removing installed files from one of the individual packages without removing any from other uhal packages
#     would be much harder to implement (especially when coping with changes to filenames between different releases)

ifndef prefix
  PYTHON_DIRECTORIES = $(shell python -c "import site,sys; print ' '.join([x+'/*uhal*' for x in site.getsitepackages()])")
else
  PYTHON_DIRECTORIES = ${prefix}/lib/python*/*-packages/*uhal* ${exec_prefix}/lib/python*/*-packages/*uhal*
endif
include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/config/mfInstallVariables.mk

.PHONY: uninstall
uninstall:
	rm -rf $(bindir)/uhal
	rm -rf $(libdir)/libcactus_uhal*
	rm -rf $(includedir)/uhal
	rm -rf $(sysconfdir)/uhal
	rm -rf $(PYTHON_DIRECTORIES)
