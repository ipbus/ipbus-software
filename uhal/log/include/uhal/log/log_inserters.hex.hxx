
#include <uhal/log/log_configuration.hpp>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

	template< typename T >
	void log_inserter ( const _Hex< T >& aHex )
	{
		fputs ( "[ " , log_configuration::getDestination() );
#ifdef __GNUG__
		// this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
		int lStatus ( 0 );
		fputs ( abi::__cxa_demangle ( typeid ( T ).name() , 0 , 0 , &lStatus ) , log_configuration::getDestination() );
#else
		fputs ( typeid ( *this ).name() , log_configuration::getDestination() );
#endif
		fputs ( " ] 0x" , log_configuration::getDestination() );
		static const char* lCharacterMapping ( "0123456789ABCDEF" );
		uint8_t* lStart ( ( uint8_t* ) ( & aHex.value() ) );
		uint8_t* lPtr ( lStart + sizeof ( T ) );

		do
		{
			--lPtr;
			fputc ( * ( lCharacterMapping + ( ( ( *lPtr ) &0xF0 ) >>4 ) ) , log_configuration::getDestination() );
			fputc ( * ( lCharacterMapping + ( ( ( *lPtr ) &0x0F ) ) ) , log_configuration::getDestination() );
		}
		while ( lPtr!=lStart );
	}


	template< typename T >
	_Hex< T > Hex ( const T& aT )
	{
		return _Hex< T > ( aT );
	}

}

