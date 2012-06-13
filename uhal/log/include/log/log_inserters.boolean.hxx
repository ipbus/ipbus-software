
#include <log/log_configuration.hpp>

namespace uhal{


	template< boolean_format FORMAT >
	_Boolean< FORMAT > Boolean( const bool& aBool ){
		return _Boolean< FORMAT >( aBool );
	}


}

