
#ifndef UHAL_PYCOHAL_EXCEPTIONS_HPP
#define UHAL_PYCOHAL_EXCEPTIONS_HPP


#include "uhal/log/exception.hpp"

namespace pybind11 {
class module_;
}

namespace pycohal {

  UHAL_DEFINE_EXCEPTION_CLASS(PycohalLogLevelEnumError, "Exception class to handle errors in translating logging levels")

  //! Wraps all uHAL exceptions (i.e. creates corresponding Python classes, and regsiters appropriate exception translators)
  void wrap_exceptions(pybind11::module_&);

}


#endif
