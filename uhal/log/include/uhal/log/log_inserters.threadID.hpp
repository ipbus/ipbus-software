
#ifndef _log_inserters_threadID_hpp_
#define _log_inserters_threadID_hpp_

#include "boost/thread.hpp" 
#include "boost/lexical_cast.hpp" 
#include <uhal/log/log_inserters.string.hpp>

namespace uhal
{

	void log_insert_threadID() 
	{ 
		log_inserter ( boost::lexical_cast<std::string>(boost::this_thread::get_id()).c_str() ); 
	} 


}

#endif
