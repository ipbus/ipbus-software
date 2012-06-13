
#include <log/log_configuration.hpp>

namespace uhal{


	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint8_t , BASE , FORMAT > Integer( const uint8_t& aInt ){
		return _Integer< uint8_t , BASE , FORMAT >( aInt );
	}

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int8_t , BASE , FORMAT > Integer( const int8_t& aInt ){
		return _Integer< int8_t , BASE , FORMAT >( aInt );
	}	

	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint16_t , BASE , FORMAT > Integer( const uint16_t& aInt ){
		return _Integer< uint16_t , BASE , FORMAT >( aInt );
	}

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int16_t , BASE , FORMAT > Integer( const int16_t& aInt ){
		return _Integer< int16_t , BASE , FORMAT >( aInt );
	}

	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint32_t , BASE , FORMAT > Integer( const uint32_t& aInt ){
		return _Integer< uint32_t , BASE , FORMAT >( aInt );
	}

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int32_t , BASE , FORMAT > Integer( const int32_t& aInt ){
		return _Integer< int32_t , BASE , FORMAT >( aInt );
	}

	template< integer_base BASE , integer_format FORMAT >
	_Integer< uint64_t , BASE , FORMAT > Integer( const uint64_t& aInt ){
		return _Integer< uint64_t , BASE , FORMAT >( aInt );
	}

	template< integer_base BASE , integer_format FORMAT >
	_Integer< int64_t , BASE , FORMAT > Integer( const int64_t& aInt ){
		return _Integer< int64_t , BASE , FORMAT >( aInt );
	}	



	template< integer_base BASE >
	_Integer< uint8_t , BASE, DefaultIntegerFormat > Integer( const uint8_t& aInt ){
		return _Integer< uint8_t , BASE, DefaultIntegerFormat >( aInt );
	}

	template< integer_base BASE >
	_Integer< int8_t , BASE, DefaultIntegerFormat > Integer( const int8_t& aInt ){
		return _Integer< int8_t , BASE, DefaultIntegerFormat >( aInt );
	}	

	template< integer_base BASE >
	_Integer< uint16_t , BASE, DefaultIntegerFormat > Integer( const uint16_t& aInt ){
		return _Integer< uint16_t , BASE, DefaultIntegerFormat >( aInt );
	}

	template< integer_base BASE >
	_Integer< int16_t , BASE, DefaultIntegerFormat > Integer( const int16_t& aInt ){
		return _Integer< int16_t , BASE, DefaultIntegerFormat >( aInt );
	}

	template< integer_base BASE >
	_Integer< uint32_t , BASE, DefaultIntegerFormat > Integer( const uint32_t& aInt ){
		return _Integer< uint32_t , BASE, DefaultIntegerFormat >( aInt );
	}

	template< integer_base BASE >
	_Integer< int32_t , BASE, DefaultIntegerFormat > Integer( const int32_t& aInt ){
		return _Integer< int32_t , BASE, DefaultIntegerFormat >( aInt );
	}

	template< integer_base BASE >
	_Integer< uint64_t , BASE, DefaultIntegerFormat > Integer( const uint64_t& aInt ){
		return _Integer< uint64_t , BASE, DefaultIntegerFormat >( aInt );
	}

	template< integer_base BASE >
	_Integer< int64_t , BASE, DefaultIntegerFormat > Integer( const int64_t& aInt ){
		return _Integer< int64_t , BASE, DefaultIntegerFormat >( aInt );
	}	



	template< typename T >
	void log_inserter( const _Integer< T , bin , fixed >& aInt )
	{
		uint32_t lSize( sizeof( T )<<3 );
		fputs( "0b" , log_configuration::getDestination() );
	
		T lValue( aInt.value() );
		T lMask( 0x1 );
		lMask <<= (lSize-1);

		for ( uint32_t i=0 ; i!=lSize ; ++i ){
			fputc( ( lValue & lMask )?'1':'0' , log_configuration::getDestination() );
			lValue <<= 1;
		}

	}



	template< typename T >
	void log_inserter( const _Integer< T , hex , fixed >& aInt )
	{
		uint32_t lSize( sizeof( T ) );
		static const char* lCharacterMapping( "0123456789ABCDEF" );

		fputs( "0x" , log_configuration::getDestination() );

		uint8_t* lStart( ( uint8_t* )( & aInt.value() ) );
		uint8_t* lPtr( lStart + lSize );

		do{
			--lPtr;
			fputc( *( lCharacterMapping + ( ((*lPtr)&0xF0)>>4 ) ) , log_configuration::getDestination() );
			fputc( *( lCharacterMapping + ( ((*lPtr)&0x0F) ) ) , log_configuration::getDestination() );			
		}while ( lPtr!=lStart );
		
	}




	template< typename T >
	void log_inserter( const _Integer< T , bin , variable >& aInt )
	{

		if( aInt.value() == 0 )
		{
			fputs( "0b0" , log_configuration::getDestination() );
		}
		else
		{
			uint32_t lSize( sizeof( T )<<3 );
			fputs( "0b" , log_configuration::getDestination() );
	
			T lValue( aInt.value() );
			T lMask( 0x1 );
			lMask <<= (lSize-1);
	
			bool lPrint( false );
			bool lCurrent( false );
	
			for ( uint32_t i=0 ; i!=lSize ; ++i ){
				lCurrent = lValue & lMask;
				if( lPrint |= lCurrent ){
					fputc( lCurrent?'1':'0' , log_configuration::getDestination() );
				}
				lValue <<= 1;
			}
		}
	}



	template< typename T >
	void log_inserter( const _Integer< T , dec , variable >& aInt )
	{
		static const char* lCharacterMapping( "9876543210123456789" );
		static const char* lCharacterMappingCenter( lCharacterMapping + 9 );

		char lBuffer[24]; //greater than the size of a 64bit decimal number
		char* lPtr = lBuffer;

		T value( aInt.value() );
		T tmp_value;

		SignHelper( value );

		do {
			tmp_value = value;
			value /= 10;
			*lPtr++ = *(lCharacterMappingCenter + tmp_value - (value * 10));
		} while ( value );
	
		do {
			fputc( *(--lPtr) , log_configuration::getDestination() );
		} while( lPtr!=lBuffer );

	}



	template< typename T >
	void log_inserter( const _Integer< T , hex , variable >& aInt )
	{
		static const char* lCharacterMapping( "0123456789ABCDEF" );

		if( aInt.value() == 0 )
		{
			fputs( "0x0" , log_configuration::getDestination() );
		}
		else
		{
			uint32_t lSize( sizeof( T ) );
			fputs( "0x" , log_configuration::getDestination() );
		
			bool lPrint( false );
			uint32_t lPos( 0 );
	
			uint8_t* lStart( ( uint8_t* )( & aInt.value() ) );
			uint8_t* lPtr( lStart + lSize );
	
			do{
				--lPtr;
				lPos = (((*lPtr)&0xF0)>>4);
				if( lPrint |= (bool)(lPos) ){
					fputc( *( lCharacterMapping + lPos ) , log_configuration::getDestination() );
				}
				lPos = ((*lPtr)&0x0F);
				if( lPrint |= (bool)(lPos) ){
					fputc( *( lCharacterMapping + lPos ) , log_configuration::getDestination() );
				}	
			}while ( lPtr!=lStart );

		}
	}




}

