#include <uhal/log/log.hpp>

#include <sys/time.h>
#include <uhal/log/log_inserters.time.hpp>

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
