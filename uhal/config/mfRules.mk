# Sanitize package path
PackagePath := $(shell cd ${PackagePath}; pwd)

# Library sources
LibrarySources = $(sort $(wildcard src/common/*.cpp) $(wildcard src/common/**/*.cpp))
# Filter undesired files
LibrarySourcesFiltered = $(filter-out ${IgnoreSources}, ${LibrarySources})
# Turn them into objects
LibraryObjectFiles = $(patsubst src/common/%.cpp,${PackagePath}/obj/%.o,${LibrarySourcesFiltered})

ExecutableSources =$(sort $(wildcard src/common/*.cxx))
# Filter undesired files
ExecutableSourcesFiltered = $(filter-out ${IgnoreSources}, ${ExecutableSources})
# Turn them into objects
ExecutableObjectFiles = $(patsubst src/common/%.cxx,${PackagePath}/obj/%.o,${ExecutableSourcesFiltered})
# And binaries
Executables = $(patsubst src/common/%.cxx,${PackagePath}/bin/%.exe,${ExecutableSourcesFiltered})

ObjectSubDirPaths = $(sort $(foreach filePath,${LibraryObjectFiles} ${ExecutableObjectFiles}, $(dir ${filePath})))

# $(info LibrarySourcesFiltered = ${LibrarySourcesFiltered})
# $(info ExecutableSourcesFiltered = ${ExecutableSourcesFiltered})
# $(info ExecutableObjectFiles = ${ExecutableObjectFiles})
# $(info Executables = ${Executables})

# Compiler Flags
IncludePaths := $(addprefix -I,${IncludePaths})


# Library dependencies
DependentLibraries += $(addprefix -L,${LibraryPaths})
DependentLibraries += $(addprefix -l,${Libraries})

# Executable dependencies
ExecutableDependentLibraries += $(addprefix -L,${LibraryPaths})
ExecutableDependentLibraries += $(addprefix -l,${ExecutableLibraries})
ExecutableDependentLibraries := $(patsubst -l-%,-%,${ExecutableDependentLibraries})


ifeq ("${Library}","")
  LibraryFile ?=
else
  ifdef BUILD_STATIC
      LibraryFile = lib/lib${Library}.a
  else
    ifeq ("${LIBRARY_VER_ABI}","")
      LibraryFile ?= lib/lib${Library}.so
    else
      ifeq ($(CACTUS_OS), osx)
        LDFLAGS_SONAME ?= -Wl,-install_name,lib${Library}.so.${LIBRARY_VER_ABI}
      else
        LDFLAGS_SONAME ?= -Wl,-soname,lib${Library}.so.${LIBRARY_VER_ABI}
      endif
      LibraryFile ?= lib/lib${Library}.so.${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}
      LibraryLinkSONAME ?= lib/lib${Library}.so.${LIBRARY_VER_ABI}
      LibraryLinkPlain ?= lib/lib${Library}.so
    endif
  endif
endif


.PHONY: default
default: build

.PHONY: clean _cleanall
clean: _cleanall
_cleanall:
	rm -rf obj
	rm -rf bin
	rm -rf lib ${LibraryFile} ${LibraryLinkSONAME} ${LibraryLinkPlain}

.PHONY: all _all build buildall
all: _all
build: _all
buildall: _all
_all: ${LibraryFile} ${Executables} ${ExtraTargets}

.PHONY: objects
objects: ${LibraryObjectFiles} ${ExecutableObjectFiles}

${PackagePath}/obj ${PackagePath}/lib ${PackagePath}/bin ${ObjectSubDirPaths}:
	${MakeDir} $@

# Implicit rule for .cpp -> .o
.SECONDEXPANSION:
${PackagePath}/obj/%.o : ${PackagePath}/src/common/%.cpp  | $$(dir ${PackagePath}/obj/%.o)
	${CXX} -c ${CXXFLAGS} ${IncludePaths} $< -o $@

# Implicit rule for .cxx -> .o
.SECONDEXPANSION:
${PackagePath}/obj/%.o : ${PackagePath}/src/common/%.cxx  | $$(dir ${PackagePath}/obj/%.o)
	${CXX} -c ${CXXFLAGS} ${IncludePaths} $< -o $@

# Main target: shared library
${LibraryFile}: ${LibraryObjectFiles}  | ${PackagePath}/lib
ifndef BUILD_STATIC
	${LD} -shared ${LDFLAGS_SONAME} ${LDFLAGS} ${LibraryObjectFiles} ${DependentLibraries} -o $@
else
	${AR} ${ARFLAGS} $@ ${LibraryObjectFiles}
endif
ifneq ("${LibraryLinkSONAME}","")
	ln -s -f ${PackagePath}/${LibraryFile} ${LibraryLinkSONAME}
ifneq ("${LibraryLinkPlain}", "")
	ln -s -f ${PackagePath}/${LibraryLinkSONAME} ${LibraryLinkPlain}
endif
endif

# Include automatically generated dependencies
-include $(LibraryObjectFiles:.o=.d)

# Static Pattern rule for binaries
${Executables} : ${PackagePath}/bin/%.exe : ${PackagePath}/obj/%.o ${LibraryFile}  | ${PackagePath}/bin
	${LD} ${LDFLAGS} $< ${ExecutableDependentLibraries} -o $@

# Include automatically generated dependencies
-include $(ExecutableObjectFiles:.o=.d)
