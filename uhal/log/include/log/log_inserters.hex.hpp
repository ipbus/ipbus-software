
#ifndef _log_inserters_hex_hpp_
#define _log_inserters_hex_hpp_

#include <log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{

	template< typename T > class _Hex;

	template< typename T > _Hex< T > Hex ( const T& aT );

	template< typename T >
	class _Hex : public RefWrapper< T >
	{
			friend _Hex< T > Hex<> ( const T& aT );
			_Hex ( const T& aT ) : RefWrapper< T > ( aT ) {}
	};




}

#include <log/log_inserters.hex.hxx>

#endif
