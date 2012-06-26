#include <uhal/log/log.hpp>

namespace uhal
{

	template< typename T >
	void log_formatter< T >::tail ( )
	{
		put ( "\033[0m\n" );
	}

	template< typename T >
	void log_formatter< T >::head ( )
	{
		template_specialization_helper< T >::print ( );
	}
	
}
