
#ifndef _log_inserters_string_hpp_
#define _log_inserters_string_hpp_

#include <log/log_inserter_helper.hpp>

#include <string>

namespace uhal
{

	//extra function needed to handle the c-style string case, for convenience.
	void log_inserter ( const char* );

}

#endif
