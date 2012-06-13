
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

	static const integer_base DefaultIntegerBase ( dec );

	enum integer_format
	{
		fixed,
		variable
	};

	static const integer_format DefaultIntegerFormat ( variable );


	template< typename T , integer_base BASE , integer_format FORMAT >
	class _Integer : public RefWrapper< T >
	{
		public:
			_Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
	};


	template< typename T >
	void SignHelper ( const T& aInt );


	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint8_t , BASE , FORMAT > Integer ( const uint8_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int8_t , BASE , FORMAT > Integer ( const int8_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint16_t , BASE , FORMAT > Integer ( const uint16_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int16_t , BASE , FORMAT > Integer ( const int16_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint32_t , BASE , FORMAT > Integer ( const uint32_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int32_t , BASE , FORMAT > Integer ( const int32_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint64_t , BASE , FORMAT > Integer ( const uint64_t& aInt );

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int64_t , BASE , FORMAT > Integer ( const int64_t& aInt );



	template< integer_base BASE >
	_Integer< uint8_t , BASE, DefaultIntegerFormat > Integer ( const uint8_t& aInt );

	template< integer_base BASE >
	_Integer< int8_t , BASE, DefaultIntegerFormat > Integer ( const int8_t& aInt );

	template< integer_base BASE >
	_Integer< uint16_t , BASE, DefaultIntegerFormat > Integer ( const uint16_t& aInt );

	template< integer_base BASE >
	_Integer< int16_t , BASE, DefaultIntegerFormat > Integer ( const int16_t& aInt );

	template< integer_base BASE >
	_Integer< uint32_t , BASE, DefaultIntegerFormat > Integer ( const uint32_t& aInt );

	template< integer_base BASE >
	_Integer< int32_t , BASE, DefaultIntegerFormat > Integer ( const int32_t& aInt );

	template< integer_base BASE >
	_Integer< uint64_t , BASE, DefaultIntegerFormat > Integer ( const uint64_t& aInt );

	template< integer_base BASE >
	_Integer< int64_t , BASE, DefaultIntegerFormat > Integer ( const int64_t& aInt );



	_Integer< uint8_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint8_t& aInt );

	_Integer< int8_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int8_t& aInt );

	_Integer< uint16_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint16_t& aInt );

	_Integer< int16_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int16_t& aInt );

	_Integer< uint32_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint32_t& aInt );

	_Integer< int32_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int32_t& aInt );

	_Integer< uint64_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint64_t& aInt );

	_Integer< int64_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int64_t& aInt );


}

#include <log/log_inserters.integer.hxx>

#endif
