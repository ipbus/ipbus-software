
#include <uhal/log/log_configuration.hpp>

namespace uhal
{


	template< typename FORMAT >
	struct IntegerFactory < uint8_t , FORMAT >
	{
		static _Integer< uint8_t , FORMAT > Construct ( const uint8_t& aInt )
		{
			return _Integer< uint8_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < int8_t , FORMAT >
	{
		static _Integer< int8_t , FORMAT > Construct ( const int8_t& aInt )
		{
			return _Integer< int8_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < uint16_t , FORMAT >
	{
		static _Integer< uint16_t , FORMAT > Construct ( const uint16_t& aInt )
		{
			return _Integer< uint16_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < int16_t , FORMAT >
	{
		static _Integer< int16_t , FORMAT > Construct ( const int16_t& aInt )
		{
			return _Integer< int16_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < uint32_t , FORMAT >
	{
		static _Integer< uint32_t , FORMAT > Construct ( const uint32_t& aInt )
		{
			return _Integer< uint32_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < int32_t , FORMAT >
	{
		static _Integer< int32_t , FORMAT > Construct ( const int32_t& aInt )
		{
			return _Integer< int32_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < uint64_t , FORMAT >
	{
		static _Integer< uint64_t , FORMAT > Construct ( const uint64_t& aInt )
		{
			return _Integer< uint64_t , FORMAT > ( aInt );
		}
	};

	template< typename FORMAT >
	struct IntegerFactory < int64_t , FORMAT >
	{
		static _Integer< int64_t , FORMAT > Construct ( const int64_t& aInt )
		{
			return _Integer< int64_t , FORMAT > ( aInt );
		}
	};




	template< typename T >
	_Integer< T , IntFmt<> > Integer ( const T& aT )
	{
		return IntegerFactory< T , IntFmt<> >::Construct ( aT );
	}

	template< typename T , typename FORMAT >
	_Integer< T , FORMAT > Integer ( const T& aT , const FORMAT& aFmt )
	{
		return IntegerFactory< T , FORMAT >::Construct ( aT );
	}






	template< typename T >
	void SignHelper ( const T& aInt )
	{}


	template< typename T , uint32_t WIDTH >
	void log_inserter ( const _Integer< T , IntFmt<bin , fixed , WIDTH> >& aInt )
	{
		uint32_t lSize ( sizeof ( T ) << 3 ); //number of characters
		fputs ( "0b" , log_configuration::getDestination() );
		int32_t i ( WIDTH-lSize );

		if ( i > 0 )
		{
			for ( ; i!=0 ; --i )
			{
				fputc ( '0' , log_configuration::getDestination() );
			}
		}

		T lValue ( aInt.value() );
		T lMask ( 0x1 );
		lMask <<= ( lSize-1 );

		for ( uint32_t i=0 ; i!=lSize ; ++i )
		{
			fputc ( ( lValue & lMask ) ?'1':'0' , log_configuration::getDestination() );
			lValue <<= 1;
		}
	}


	template< typename T , uint32_t WIDTH >
	void log_inserter ( const _Integer< T , IntFmt<dec , fixed , WIDTH> >& aInt )
	{
		static const char* lCharacterMapping ( "9876543210123456789" );
		static const char* lCharacterMappingCenter ( lCharacterMapping + 9 );
		char lBuffer[24]; //greater than the size of a 64bit decimal number
		char* lPtr = lBuffer;
		T value ( aInt.value() );
		T tmp_value;
		SignHelper ( value );

		do
		{
			tmp_value = value;
			value /= 10;
			*lPtr++ = * ( lCharacterMappingCenter + tmp_value - ( value * 10 ) );
		}
		while ( value );

		int32_t i ( WIDTH- ( lPtr-lBuffer ) );

		if ( i > 0 )
		{
			for ( ; i!=0 ; --i )
			{
				fputc ( '0' , log_configuration::getDestination() );
			}
		}

		do
		{
			fputc ( * ( --lPtr ) , log_configuration::getDestination() );
		}
		while ( lPtr!=lBuffer );
	}


	template< typename T , uint32_t WIDTH >
	void log_inserter ( const _Integer< T , IntFmt<hex , fixed , WIDTH> >& aInt )
	{
		uint32_t lSize ( sizeof ( T ) << 1 ); //number of characters
		static const char* lCharacterMapping ( "0123456789ABCDEF" );
		fputs ( "0x" , log_configuration::getDestination() );
		int32_t i ( WIDTH-lSize );

		if ( i > 0 )
		{
			for ( ; i!=0 ; --i )
			{
				fputc ( '0' , log_configuration::getDestination() );
			}
		}

		uint8_t* lStart ( ( uint8_t* ) ( & aInt.value() ) );
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
	void log_inserter ( const _Integer< T , IntFmt<bin , variable , 0> >& aInt )
	{
		if ( aInt.value() == 0 )
		{
			fputs ( "0b0" , log_configuration::getDestination() );
		}
		else
		{
			uint32_t lSize ( sizeof ( T ) <<3 );
			fputs ( "0b" , log_configuration::getDestination() );
			T lValue ( aInt.value() );
			T lMask ( 0x1 );
			lMask <<= ( lSize-1 );
			bool lPrint ( false );
			bool lCurrent ( false );

			for ( uint32_t i=0 ; i!=lSize ; ++i )
			{
				lCurrent = lValue & lMask;

				if ( lPrint |= lCurrent )
				{
					fputc ( lCurrent?'1':'0' , log_configuration::getDestination() );
				}

				lValue <<= 1;
			}
		}
	}



	template< typename T >
	void log_inserter ( const _Integer< T , IntFmt<dec , variable , 0> >& aInt )
	{
		static const char* lCharacterMapping ( "9876543210123456789" );
		static const char* lCharacterMappingCenter ( lCharacterMapping + 9 );
		char lBuffer[24]; //greater than the size of a 64bit decimal number
		char* lPtr = lBuffer;
		T value ( aInt.value() );
		T tmp_value;
		SignHelper ( value );

		do
		{
			tmp_value = value;
			value /= 10;
			*lPtr++ = * ( lCharacterMappingCenter + tmp_value - ( value * 10 ) );
		}
		while ( value );

		do
		{
			fputc ( * ( --lPtr ) , log_configuration::getDestination() );
		}
		while ( lPtr!=lBuffer );
	}



	template< typename T >
	void log_inserter ( const _Integer< T , IntFmt<hex , variable , 0> >& aInt )
	{
		static const char* lCharacterMapping ( "0123456789ABCDEF" );

		if ( aInt.value() == 0 )
		{
			fputs ( "0x0" , log_configuration::getDestination() );
		}
		else
		{
			uint32_t lSize ( sizeof ( T ) );
			fputs ( "0x" , log_configuration::getDestination() );
			bool lPrint ( false );
			uint32_t lPos ( 0 );
			uint8_t* lStart ( ( uint8_t* ) ( & aInt.value() ) );
			uint8_t* lPtr ( lStart + lSize );

			do
			{
				--lPtr;
				lPos = ( ( ( *lPtr ) &0xF0 ) >>4 );

				if ( lPrint |= ( bool ) ( lPos ) )
				{
					fputc ( * ( lCharacterMapping + lPos ) , log_configuration::getDestination() );
				}

				lPos = ( ( *lPtr ) &0x0F );

				if ( lPrint |= ( bool ) ( lPos ) )
				{
					fputc ( * ( lCharacterMapping + lPos ) , log_configuration::getDestination() );
				}
			}
			while ( lPtr!=lStart );
		}
	}




}

