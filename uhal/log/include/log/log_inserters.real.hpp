
#ifndef _log_inserters_real_hpp_
#define _log_inserters_real_hpp_

#include <log/log_inserter_helper.hpp> 

#include <stdint.h>

namespace uhal{

	static const uint32_t DefaultRealWidth( 10 );

	template< typename T , uint32_t MAX_WIDTH >
	class _Real : public RefWrapper< T >
	{	
		public:
			_Real( const T& aReal ) : RefWrapper< T >( aReal ){}
	};

	_Real< double , DefaultRealWidth > Real( const double& aReal );
	_Real< float , DefaultRealWidth > Real( const float& aReal );

	template< uint32_t MAX_WIDTH >
	_Real< double , MAX_WIDTH > Real( const double & aReal );

	template< uint32_t MAX_WIDTH >
	_Real< float , MAX_WIDTH > Real( const float & aReal );

}

#include <log/log_inserters.real.hxx>

#endif
