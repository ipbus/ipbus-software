
#include <uhal/log/log_inserters.location.hpp>
#include <uhal/log/log_inserters.integer.hpp>

#include <uhal/log/log_configuration.hpp>

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
		fputs ( "function \"" , log_configuration::getDestination() );
		fputs ( aLocation.mFunction , log_configuration::getDestination() );
		fputs ( "\" in " , log_configuration::getDestination() );
		fputs ( aLocation.mFile , log_configuration::getDestination() );
		fputs ( ", line " , log_configuration::getDestination() );
		log_inserter ( Integer ( aLocation.mLine ) );
		fputc ( '.' , log_configuration::getDestination() );
	}

}
