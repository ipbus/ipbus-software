
#include <uhal/log/log.hpp>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

	template< typename T >
	void log_inserter ( const _Hex< T >& aHex )
	{
		put ( "[ " );
#ifdef __GNUG__
		// this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
		int lStatus ( 0 );
		put ( abi::__cxa_demangle ( typeid ( T ).name() , 0 , 0 , &lStatus ) );
#else
		put ( typeid ( *this ).name() );
#endif
		put ( " ] 0x" );
		static const char* lCharacterMapping ( "0123456789ABCDEF" );
		uint8_t* lStart ( ( uint8_t* ) ( & aHex.value() ) );
		uint8_t* lPtr ( lStart + sizeof ( T ) );

		do
		{
			--lPtr;
			put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0xF0 ) >>4 ) ) );
			put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0x0F ) ) ) );
		}
		while ( lPtr!=lStart );
	}


	template< typename T >
	_Hex< T > Hex ( const T& aT )
	{
		return _Hex< T > ( aT );
	}

}

