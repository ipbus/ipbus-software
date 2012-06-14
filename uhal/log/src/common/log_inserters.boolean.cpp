
#include <log/log_inserters.boolean.hpp>

#include <log/log_configuration.hpp>

namespace uhal
{

	template<>
	void log_inserter< _Boolean< bool , BoolFmt<alpha> > > ( const _Boolean< bool , BoolFmt<alpha> >& aBoolean )
	{
		if ( aBoolean.value() )
		{
			fputs ( "True" , log_configuration::getDestination() );
		}
		else
		{
			fputs ( "False" , log_configuration::getDestination() );
		}
	}


	template<>
	void log_inserter< _Boolean< bool , BoolFmt<numeric> > > ( const _Boolean< bool , BoolFmt<numeric> >& aBoolean )
	{
		if ( aBoolean.value() )
		{
			fputc ( '1' , log_configuration::getDestination() );
		}
		else
		{
			fputc ( '0' , log_configuration::getDestination() );
		}
	}



}
