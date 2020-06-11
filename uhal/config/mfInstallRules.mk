
include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/mfInstallVariables.mk

.PHONY: install

install:
	if [ -d bin     ]; then cd bin    ; find . -type f -exec install -D -m 755 {} ${bindir}/${Project}/{} \; ; fi
	if [ -d scripts ]; then cd scripts; find . -type f -exec install -D -m 755 {} ${bindir}/${Project}/{} \; ; fi
	if [ -d lib     ]; then cd lib    ; find . -type f -exec install -D -m 755 {} ${libdir}/{} \; ; fi
	if [ -d lib     ]; then cd lib    ; find . -type l -exec sh -c 'ln -s -f $$(basename $$(readlink $$0)) ${libdir}/$$0' {} \; ; fi
	if [ -d include ]; then cd include; find . -type f -exec install -D -m 644 {} ${includedir}/{} \; ; fi
	if [ -d etc     ]; then cd etc    ; find . -type f -exec install -D -m 644 {} ${sysconfdir}/{} \; ; fi

