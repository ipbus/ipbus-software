#
# Makefile for uhal tests
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = uhal/tests
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactuscore-uhal-tests

Packager = Andrew Rose, Marc Magrans de Arbil

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 4
PACKAGE_RELEASE = 1

LIBRARY = lib/libcactus_uhal_tests.so
LIBRARY_SOURCES = $(wildcard src/common/*.cpp)
LIBRARY_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${LIBRARY_SOURCES})

EXECUTABLE_SOURCES = \
		src/common/DummyHardwareUdp.cxx \
		src/common/DummyHardwareTcp.cxx \
		src/common/WiresharkParser.cxx \
		src/common/test_log.cxx \
		src/common/test_dummy_single.cxx \
		src/common/test_dummy_masking.cxx \
		src/common/test_dummy_check_permissions.cxx \
		src/common/test_dummy_docu_examples.cxx \
		src/common/test_dummy_block.cxx \
		src/common/test_dummy_hierarchy.cxx \
		src/common/test_dummy_multithreaded.cxx \
		src/common/test_dummy_timeout.cxx \
		src/common/test_dummy_nonreachable.cxx \
		src/common/test_dummy_metainfo.cxx \
		src/common/test_dummy_navigation.cxx \
		src/common/test_dummy_rawclient.cxx \
		src/common/PerfTester.cxx

EXECUTABLE_OBJECT_FILES = $(patsubst src/common/%.cxx,obj/%.o,${EXECUTABLE_SOURCES})
EXECUTABLES = $(patsubst src/common/%.cxx,bin/%.exe,${EXECUTABLE_SOURCES})


LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX} \
		-L${EXTERN_PUGIXML_LIB_PREFIX} \
		-L${UHAL_LOG_LIB_PREFIX} \
		-L${UHAL_GRAMMARS_LIB_PREFIX} \
		-L${UHAL_UHAL_LIB_PREFIX} \
		-Llib 

LIBRARIES = 	\
		-lpthread \
		\
		-lboost_thread \
		-lboost_filesystem \
		-lboost_regex \
		-lboost_system \
		-lboost_thread \
		-lboost_program_options \
		\
		-lcactus_extern_pugixml \
		-lcactus_uhal_log \
		-lcactus_uhal_grammars \
		-lcactus_uhal_uhal	

EXECUTABLE_LIBRARIES = ${LIBRARIES} -lcactus_uhal_tests

INCLUDE_PATH = 	-Iinclude  \
		-I${UHAL_LOG_INCLUDE_PREFIX} \
		-I${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		-I${UHAL_UHAL_INCLUDE_PREFIX}

CPP_FLAGS = -g -Wall -O3 -MMD -MP -fPIC ${INCLUDE_PATH}

LINK_LIBRARY_FLAGS = -shared -fPIC -Wall -g -O3 ${LIBRARY_PATH} ${LIBRARIES}

LINK_EXECUTABLE_FLAGS = -Wall -g -O3 ${LIBRARY_PATH} ${EXECUTABLE_LIBRARIES} 

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf bin
	rm -rf lib

all: _all
build: _all
buildall: _all
_all: make_dirs ${LIBRARY} ${EXECUTABLES} copy_to_build

make_dirs:
	mkdir -p {bin,obj,lib}
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{bin/uhal/tests,lib,include/uhal/tests,etc/uhal/tests}

${EXECUTABLES}: bin/%.exe: obj/%.o ${EXECUTABLE_OBJECT_FILES}
	g++ ${LINK_EXECUTABLE_FLAGS} $< -o $@

${EXECUTABLE_OBJECT_FILES}: obj/%.o : src/common/%.cxx
	g++ -c ${CPP_FLAGS}  $< -o $@
-include $(EXECUTABLE_SOURCES:.cxx=.d)

${LIBRARY}: ${LIBRARY_OBJECT_FILES}
	g++ ${LINK_LIBRARY_FLAGS} ${LIBRARY_OBJECT_FILES} -o $@

${LIBRARY_OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	g++ -c ${CPP_FLAGS} $< -o $@
-include $(LIBRARY_SOURCES:.cpp=.d)

copy_to_build:
	cp -p include/uhal/tests/*.hpp ${RPMBUILD_DIR}/SOURCES/include/uhal/tests/.
	cp -p bin/*.exe ${RPMBUILD_DIR}/SOURCES/bin/uhal/tests/.
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p etc/uhal/tests/*.xml ${RPMBUILD_DIR}/SOURCES/etc/uhal/tests

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactuscore-uhal-tests.spec

