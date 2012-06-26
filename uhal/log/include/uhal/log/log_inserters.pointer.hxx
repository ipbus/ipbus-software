
#include <uhal/log/log.hpp>
#include <uhal/log/log_inserters.integer.hpp>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

	template< typename T >
	void log_inserter ( const _Pointer< T >& aPointer )
	{
		put ( '(' );
#ifdef __GNUG__
		// this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
		int lStatus ( 0 );
		put ( abi::__cxa_demangle ( typeid ( T* ).name() , 0 , 0 , &lStatus ) );
#else
		put ( typeid ( *this ).name() );
#endif
		put ( ")(0x" );
		static const char* lCharacterMapping ( "0123456789ABCDEF" );
		uint64_t lPointer ( ( uint64_t ) ( aPointer.value() ) );
		uint8_t* lStart ( ( uint8_t* ) ( &lPointer ) );
		uint8_t* lPtr ( lStart + sizeof ( T* ) );

		do
		{
			--lPtr;
			put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0xF0 ) >>4 ) ) );
			put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0x0F ) ) ) );
		}
		while ( lPtr!=lStart );

		put ( ")(+" );
		log_inserter ( Integer ( sizeof ( T ) ) );
		put ( ')' );
	}


	template< typename T >
	_Pointer< T > Pointer ( const T* aT )
	{
		return _Pointer< T > ( aT );
	}

}

