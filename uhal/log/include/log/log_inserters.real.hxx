
#include <log/log_configuration.hpp>

#include <stdlib.h>

namespace uhal{

	template< typename T , uint32_t MAX_WIDTH >
	void log_inserter( const _Real< T , MAX_WIDTH >& aReal )
	{
		char lBuffer[ MAX_WIDTH ];
		gcvt ( aReal.value() , MAX_WIDTH, lBuffer );
		fputs ( lBuffer , log_configuration::getDestination() );
	}


	template< uint32_t MAX_WIDTH >
	_Real< double , MAX_WIDTH > Real( const double & aReal )
	{
		return _Real< double , MAX_WIDTH >( aReal );
	}


	template< uint32_t MAX_WIDTH >
	_Real< float , MAX_WIDTH > Real( const float & aReal )
	{
		return _Real< float , MAX_WIDTH >( aReal );
	}


}

