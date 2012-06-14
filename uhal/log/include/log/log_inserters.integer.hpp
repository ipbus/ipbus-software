
#ifndef _log_inserters_integer_hpp_
#define _log_inserters_integer_hpp_

#include <log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{


	enum integer_base
	{
		bin,
		dec,
		hex
	};

	enum integer_format
	{
		fixed,
		variable
	};

	static const integer_base DefaultIntegerBase ( dec );
	static const integer_format DefaultIntegerFormat ( variable );

	template< typename T , typename FORMAT > struct IntegerFactory;

	template< integer_base BASE = DefaultIntegerBase , integer_format FORMAT = DefaultIntegerFormat , uint32_t WIDTH = 0 > struct IntFmt {};


	template< typename T , typename FORMAT >
	class _Integer : public RefWrapper< T >
	{
			friend class IntegerFactory< T , FORMAT >;
			_Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
	};

	template< typename T > void SignHelper ( const T& aInt );

	template< typename T > _Integer< T , IntFmt<> > Integer ( const T& aT );

	template< typename T , typename FORMAT > _Integer< T , FORMAT >Integer ( const T& aT , const FORMAT& aFmt );


}

#include <log/log_inserters.integer.hxx>

#endif
