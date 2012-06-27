#include <uhal/log/log.hpp>

namespace uhal
{

	template< typename T >
	void log_tail ( )
	{
		put ( "\033[0m\n" );
	}

	template< typename T >
	void log_head ( )
	{
		log_head_template_specialization_helper< T >::print ( );
	}
	
}
