
#include <uhal/log/log_inserters.location.hpp>
#include <uhal/log/log_inserters.integer.hpp>

#include <uhal/log/log.hpp>

namespace uhal
{

  Location::Location ( const char* aFunction , const char* aFile , const uint32_t& aLine ) :
    mFunction ( aFunction ),
    mFile ( aFile ) ,
    mLine ( aLine )
  {}

  template<>
  void log_inserter< Location > ( const Location& aLocation )
  {
    put ( "function \"" );
    put ( aLocation.mFunction );
    put ( "\" in " );
    put ( aLocation.mFile );
    put ( ", line " );
    log_inserter ( Integer ( aLocation.mLine ) );
    put ( '.' );
  }

}
