
#include <uhal/log/log_configuration.hpp>

#include <stdlib.h>

namespace uhal
{

	template< typename T , uint32_t WIDTH >
	void log_inserter ( const _Real< T , RealFmt<WIDTH> >& aReal )
	{
		char lBuffer[ WIDTH ];
		double lDouble ( aReal.value() ); //it appears that some platforms require this to prevent a segfault
		gcvt ( lDouble , WIDTH , lBuffer );
		fputs ( lBuffer , log_configuration::getDestination() );
	}

	template< typename FORMAT >
	struct RealFactory < double , FORMAT >
	{
		static _Real< double , FORMAT > Construct ( const double& aReal )
		{
			return _Real< double , FORMAT > ( aReal );
		}
	};

	template< typename FORMAT >
	struct RealFactory < float , FORMAT >
	{
		static _Real< float , FORMAT > Construct ( const float& aReal )
		{
			return _Real< float , FORMAT > ( aReal );
		}
	};


	template< typename T >
	_Real< T , RealFmt<> > Real ( const T& aT )
	{
		return RealFactory< T , RealFmt<> >::Construct ( aT );
	}


	template< typename T , typename FORMAT >
	_Real< T , FORMAT > Real ( const T& aT , const FORMAT& aFmt )
	{
		return RealFactory< T , FORMAT >::Construct ( aT );
	}

}

