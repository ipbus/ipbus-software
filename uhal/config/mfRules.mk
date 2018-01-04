# Sanitize package path
PackagePath := $(shell cd ${PackagePath}; pwd)

# Library sources 
LibrarySources = $(wildcard src/common/*.cpp) $(wildcard src/common/**/*.cpp)
# Filter undesired files
LibrarySourcesFiltered = $(filter-out ${IgnoreSources}, ${LibrarySources})
# Turn them into objects
LibraryObjectFiles = $(patsubst src/common/%.cpp,${PackagePath}/obj/%.o,${LibrarySourcesFiltered})

ExecutableSources = $(wildcard src/common/*.cxx)
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


ifeq ("${Library}","")
  LibraryFile ?=
else
  ifeq ("${LIBRARY_VER_ABI}","")
    LibraryFile ?= lib/lib${Library}.so
  else
    LDFLAGS_SONAME ?= -Wl,-soname,lib${Library}.so.${LIBRARY_VER_ABI}
    LibraryFile ?= lib/lib${Library}.so.${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}
    LibraryLinkSONAME ?= lib/lib${Library}.so.${LIBRARY_VER_ABI}
    LibraryLinkPlain ?= lib/lib${Library}.so
  endif
endif

.PHONY: default
default: build

.PHONY: clean _cleanall
clean: _cleanall
_cleanall:
	rm -rf obj
	rm -rf bin
	rm -rf lib ${LibraryFile} ${LibraryLink}

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
	${CPP} -c ${CXXFLAGS} ${IncludePaths} $< -o $@

# Implicit rule for .cxx -> .o 
.SECONDEXPANSION:
${PackagePath}/obj/%.o : ${PackagePath}/src/common/%.cxx  | $$(dir ${PackagePath}/obj/%.o)
	${CPP} -c ${CXXFLAGS} ${IncludePaths} $< -o $@
	
# Main target: shared library
${LibraryFile}: ${LibraryObjectFiles}  | ${PackagePath}/lib
	${LD} -shared ${LDFLAGS_SONAME} ${LDFLAGS} ${LibraryObjectFiles} ${DependentLibraries} -o $@
ifneq ("${LibraryLinkSONAME}","")
	ln -s ${PackagePath}/${LibraryFile} ${LibraryLinkSONAME}
ifneq ("${LibraryLinkPlain}", "")
	ln -s ${PackagePath}/${LibraryLinkSONAME} ${LibraryLinkPlain}
endif
endif

# Include automatically generated dependencies
-include $(LibraryObjectFiles:.o=.d)
	
# Static Pattern rule for binaries
${Executables} : ${PackagePath}/bin/%.exe : ${PackagePath}/obj/%.o ${LibraryFile}  | ${PackagePath}/bin
	${LD} ${LDFLAGS} $< ${ExecutableDependentLibraries} -o $@

# Include automatically generated dependencies
-include $(ExecutableObjectFiles:.o=.d)
