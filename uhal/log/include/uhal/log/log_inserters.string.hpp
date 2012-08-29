
#ifndef _log_inserters_string_hpp_
#define _log_inserters_string_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <string>

namespace uhal
{

  /**
  	The log_inserter function needed to handle the null-terminated, c-style strings
  	A special case since everything else is passed by reference and is handled by template functions
  	@param aStr a null-terminated string to add to the log output
  */
  void log_inserter ( const char* aStr );

}

#endif
