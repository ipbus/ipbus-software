# Sanitize BUILD_HOME
BUILD_HOME := $(shell cd $(BUILD_HOME); pwd)

$(info Using BUILD_HOME=${BUILD_HOME})

# Compilers
CPP = g++
LD = g++

# Compiler flags
CXXFLAGS = -g -Wall -pedantic -O3 -MMD -MP -fPIC
#						-fprofile-arcs -ftest-coverages
LDFLAGS = -Wall -g -O3 -fPIC
#										 -lgcov -coverage

# Tools
MakeDir = mkdir -p


PYTHON_VERSION ?= $(shell python -c "import distutils.sysconfig;print distutils.sysconfig.get_python_version()")
