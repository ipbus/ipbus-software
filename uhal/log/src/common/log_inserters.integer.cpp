
#include <uhal/log/log_inserters.integer.hpp>

namespace uhal
{

	template< >
	void SignHelper< int8_t > ( const int8_t& aInt )
	{
		if ( aInt < int8_t ( 0 ) )
		{
			put ( '-' );
		}
	}

	template< >
	void SignHelper< int16_t > ( const int16_t& aInt )
	{
		if ( aInt < int16_t ( 0 ) )
		{
			put ( '-' );
		}
	}

	template< >
	void SignHelper< int32_t > ( const int32_t& aInt )
	{
		if ( aInt < int32_t ( 0 ) )
		{
			put ( '-' );
		}
	}

	template< >
	void SignHelper< int64_t > ( const int64_t& aInt )
	{
		if ( aInt < int64_t ( 0 ) )
		{
			put ( '-' );
		}
	}


}
