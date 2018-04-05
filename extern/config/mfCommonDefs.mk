
CPP = g++

# Construct C++ compiler suffix for RPM release field (tested with clang & gcc)
CPP_VERSION_TAG = $(word 1, $(shell ${CPP} --version))
CPP_VERSION_TAG := $(subst g++,gcc,${CPP_VERSION_TAG})
CPP_VERSION_TAG := ${CPP_VERSION_TAG}$(shell ${CPP} -dumpfullversion -dumpversion)
CPP_VERSION_TAG := $(subst .,_,${CPP_VERSION_TAG})
