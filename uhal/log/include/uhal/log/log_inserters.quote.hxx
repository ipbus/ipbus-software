
#include <uhal/log/log.hpp>
#include <uhal/log/log_inserters.string.hpp>


namespace uhal
{

	template< typename T >
	void log_inserter ( const _Quote< T >& aQuote )
	{
		put ( 0x22 );
		log_inserter ( aQuote.value() );
		put ( 0x22 );
	}


	template< typename T >
	_Quote< T > Quote ( const T& aT )
	{
		return _Quote< T > ( aT );
	}

}

