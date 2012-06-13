
#include <log/log_inserters.real.hpp>

namespace uhal{


	_Real< double , DefaultRealWidth > Real( const double & aReal ){
		return _Real< double , DefaultRealWidth >( aReal );
	}

	_Real< float , DefaultRealWidth > Real( const float & aReal ){
		return _Real< float , DefaultRealWidth >( aReal );
	}


}
