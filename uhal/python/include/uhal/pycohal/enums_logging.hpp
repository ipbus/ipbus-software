#ifndef _uhal_pycohal_enums_logging_hpp_
#define _uhal_pycohal_enums_logging_hpp_


namespace pybind11 {
class module_;
}

namespace pycohal
{
  /// Log level enum - switch to using enums for setting log-levels in python
  enum LogLevel { DEBUG, INFO, NOTICE, WARNING, ERROR, FATAL };

  /// Wrapper function for uhal::setLogLevelTo - converts pycohal::LogLevel enum values to C++ uhal log-level classes
  void setLogLevelTo ( const pycohal::LogLevel& logLevel );

  /// Wrapper function for uhal::LoggingIncludes - converts pycohal::LogLevel enum values to C++ uhal log-level classes
  const bool& LoggingIncludes ( const pycohal::LogLevel& logLevel );

  void wrap_enums(pybind11::module_&);

  void wrap_logging_functions(pybind11::module_&);
}


#endif

