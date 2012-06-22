
#include <uhal/log/log_configuration.hpp>
#include <uhal/log/log_inserters.string.hpp>


namespace uhal
{

	template< typename T >
	void log_inserter ( const _Quote< T >& aQuote )
	{
		fputc ( 0x22 , log_configuration::getDestination() );
		log_inserter( aQuote.value() );
		fputc ( 0x22 , log_configuration::getDestination() );
	}


	template< typename T >
	_Quote< T > Quote ( const T& aT )
	{
		return _Quote< T > ( aT );
	}

}

