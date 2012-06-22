
#ifndef _log_inserters_quote_hpp_
#define _log_inserters_quote_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{

	template< typename T > class _Quote;

	template< typename T > _Quote< T > Quote ( const T& aT );

	_Quote< const char* > Quote ( const char* aStr );

	template< typename T >
	class _Quote : public RefWrapper< T >
	{
			friend _Quote< T > Quote<> ( const T& aT );
			friend _Quote< const char* > Quote ( const char* aStr );
			_Quote ( const T& aT ) : RefWrapper< T > ( aT ) {}
	};


}

#include <uhal/log/log_inserters.quote.hxx>

#endif
