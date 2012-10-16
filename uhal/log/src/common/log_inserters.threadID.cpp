#include <uhal/log/log_inserters.threadID.hpp>

#include "boost/thread.hpp"
#include "boost/lexical_cast.hpp"

namespace uhal
{

  void log_insert_threadID()
  {
    log_inserter ( boost::lexical_cast<std::string> ( boost::this_thread::get_id() ).c_str() );
  }

}
