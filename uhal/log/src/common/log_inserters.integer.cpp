
#include <log/log_inserters.integer.hpp>

namespace uhal
{


	_Integer< uint8_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint8_t& aInt )
	{
		return _Integer< uint8_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< int8_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int8_t& aInt )
	{
		return _Integer< int8_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< uint16_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint16_t& aInt )
	{
		return _Integer< uint16_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< int16_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int16_t& aInt )
	{
		return _Integer< int16_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< uint32_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint32_t& aInt )
	{
		return _Integer< uint32_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< int32_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int32_t& aInt )
	{
		return _Integer< int32_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< uint64_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const uint64_t& aInt )
	{
		return _Integer< uint64_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}

	_Integer< int64_t , DefaultIntegerBase , DefaultIntegerFormat > Integer ( const int64_t& aInt )
	{
		return _Integer< int64_t , DefaultIntegerBase , DefaultIntegerFormat > ( aInt );
	}


	template< >
	void SignHelper< uint8_t > ( const uint8_t& aInt )
	{
	}

	template< >
	void SignHelper< int8_t > ( const int8_t& aInt )
	{
		if ( aInt < int8_t ( 0 ) )
		{
			fputc ( '-' , log_configuration::getDestination() );
		}
	}

	template< >
	void SignHelper< uint16_t > ( const uint16_t& aInt )
	{
	}

	template< >
	void SignHelper< int16_t > ( const int16_t& aInt )
	{
		if ( aInt < int16_t ( 0 ) )
		{
			fputc ( '-' , log_configuration::getDestination() );
		}
	}

	template< >
	void SignHelper< uint32_t > ( const uint32_t& aInt )
	{
	}

	template< >
	void SignHelper< int32_t > ( const int32_t& aInt )
	{
		if ( aInt < int32_t ( 0 ) )
		{
			fputc ( '-' , log_configuration::getDestination() );
		}
	}

	template< >
	void SignHelper< uint64_t > ( const uint64_t& aInt )
	{
	}

	template< >
	void SignHelper< int64_t > ( const int64_t& aInt )
	{
		if ( aInt < int64_t ( 0 ) )
		{
			fputc ( '-' , log_configuration::getDestination() );
		}
	}


}
