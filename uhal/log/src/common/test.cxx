/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#include "log/log.hpp"
#include <iostream>

using namespace uhal;

template< typename T >
struct TestStruct{
	T mA;
	T mB;
	T mC;
	T mD;
};


int main ( int argc,char* argv[] )
{
	try
	{

// Test basic inserters
		log( Notice() , "const char*" );
		log( Notice() , std::string("std::string") );
		log( Notice() , 'c' );

		log( Notice() , "a" , 'b' , std::string("c") , "d" , "e" , "f" , "g" , "h" , "i" , "j" , "k" , "l" , "m" , "n" , "o" , "p" , "q" , "r" , "s" , "t" , "u" , "v" , "w" , "x" , "y" , "z" , "0" , "1" , "2" , "3" , "4" , "5" );


		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log( Notice() , Time< day,'/',mth,'/',year,' ',hr,':',min,':',sec,'.',usec >( lTime ) , " truncated to the nearest usecond" );
		log( Notice() , Time< day,'/',mth,'/',year,' ',hr,':',min,':',sec >( lTime ) , " truncated to the nearest second" );
		log( Notice() , Time< day,'/',mth,'/',year,' ',hr,':',min >( lTime ) , " truncated to the nearest minute" );
		log( Notice() , Time< day,'/',mth,'/',year,' ',hr >( lTime ) , " truncated to the nearest hour" );
		log( Notice() , Time< day,'/',mth,'/',year >( lTime ) , " truncated to the nearest day" );
		log( Notice() , Time< year,'/',mth,'/',day >( lTime ) , " truncated to the nearest day (US format)" );
		log( Notice() , Time< mth,'/',year >( lTime ) , " truncated to the nearest month" );
		log( Notice() , Time< strmth,' ',year >( lTime ) , " truncated to the nearest month" );
		log( Notice() , Time< year >( lTime ) , " truncated to the nearest year (4 digit format)" );
		log( Notice() , Time< yr >( lTime ) , " truncated to the nearest year (2 digit format)" );
		log( Notice() , Time< usec >( lTime ) , " microseconds" );


 		log( Notice() , "13 as a decimal uint8 : " , Integer( uint8_t(13) ) );
 		log( Notice() , "-13 as a decimal int8 : " , Integer( int8_t(-13) ) );
 		log( Notice() , "13 as a decimal uint16 : " , Integer( uint16_t(13) ) );
 		log( Notice() , "-13 as a decimal int16 : " , Integer( int16_t(-13) ) );
 		log( Notice() , "13 as a decimal uint32 : " , Integer( uint32_t(13) ) );
 		log( Notice() , "-13 as a decimal int32 : " , Integer( int32_t(-13) ) );
 		log( Notice() , "13 as a decimal uint64 : " , Integer( uint64_t(13) ) );
 		log( Notice() , "-13 as a decimal int64 : " , Integer( int64_t(-13) ) );

 		log( Notice() , "13 as a variable-width binary uint8 : " , Integer<bin>( uint8_t(13) ) );
 		log( Notice() , "-13 as a variable-width binary int8 : " , Integer<bin>( int8_t(-13) ) );
 		log( Notice() , "13 as a variable-width binary uint16 : " , Integer<bin>( uint16_t(13) ) );
 		log( Notice() , "-13 as a variable-width binary int16 : " , Integer<bin>( int16_t(-13) ) );
 		log( Notice() , "13 as a variable-width binary uint32 : " , Integer<bin>( uint32_t(13) ) );
 		log( Notice() , "-13 as a variable-width binary int32 : " , Integer<bin>( int32_t(-13) ) );
 		log( Notice() , "13 as a variable-width binary uint64 : " , Integer<bin>( uint64_t(13) ) );
 		log( Notice() , "-13 as a variable-width binary int64 : " , Integer<bin>( int64_t(-13) ) );

 		log( Notice() , "13 as a variable-width hex uint8 : " , Integer<hex>( uint8_t(13) ) );
 		log( Notice() , "-13 as a variable-width hex int8 : " , Integer<hex>( int8_t(-13) ) );
 		log( Notice() , "13 as a variable-width hex uint16 : " , Integer<hex>( uint16_t(13) ) );
 		log( Notice() , "-13 as a variable-width hex int16 : " , Integer<hex>( int16_t(-13) ) );
 		log( Notice() , "13 as a variable-width hex uint32 : " , Integer<hex>( uint32_t(13) ) );
 		log( Notice() , "-13 as a variable-width hex int32 : " , Integer<hex>( int32_t(-13) ) );
 		log( Notice() , "13 as a variable-width hex uint64 : " , Integer<hex>( uint64_t(13) ) );
 		log( Notice() , "-13 as a variable-width hex int64 : " , Integer<hex>( int64_t(-13) ) );

 		log( Notice() , "13 as a fixed-width binary uint8 : " , Integer<bin , fixed>( uint8_t(13) ) );
 		log( Notice() , "-13 as a fixed-width binary int8 : " , Integer<bin , fixed>( int8_t(-13) ) );
 		log( Notice() , "13 as a fixed-width binary uint16 : " , Integer<bin , fixed>( uint16_t(13) ) );
 		log( Notice() , "-13 as a fixed-width binary int16 : " , Integer<bin , fixed>( int16_t(-13) ) );
 		log( Notice() , "13 as a fixed-width binary uint32 : " , Integer<bin , fixed>( uint32_t(13) ) );
 		log( Notice() , "-13 as a fixed-width binary int32 : " , Integer<bin , fixed>( int32_t(-13) ) );
 		log( Notice() , "13 as a fixed-width binary uint64 : " , Integer<bin , fixed>( uint64_t(13) ) );
 		log( Notice() , "-13 as a fixed-width binary int64 : " , Integer<bin , fixed>( int64_t(-13) ) );

 		log( Notice() , "13 as a fixed-width hex uint8 : " , Integer<hex , fixed>( uint8_t(13) ) );
 		log( Notice() , "-13 as a fixed-width hex int8 : " , Integer<hex , fixed>( int8_t(-13) ) );
 		log( Notice() , "13 as a fixed-width hex uint16 : " , Integer<hex , fixed>( uint16_t(13) ) );
 		log( Notice() , "-13 as a fixed-width hex int16 : " , Integer<hex , fixed>( int16_t(-13) ) );
 		log( Notice() , "13 as a fixed-width hex uint32 : " , Integer<hex , fixed>( uint32_t(13) ) );
 		log( Notice() , "-13 as a fixed-width hex int32 : " , Integer<hex , fixed>( int32_t(-13) ) );
 		log( Notice() , "13 as a fixed-width hex uint64 : " , Integer<hex , fixed>( uint64_t(13) ) );
 		log( Notice() , "-13 as a fixed-width hex int64 : " , Integer<hex , fixed>( int64_t(-13) ) );

		log( Notice() , "double pi : " , Real( double(3.1415926535) ) );
		log( Notice() , "float pi : " , Real( float(3.1415926535) ) , " (should be truncated)" );
		log( Notice() , "double pi : " , Real<4>( double(3.1415926535) ) );
		log( Notice() , "float pi : " , Real<4>( float(3.1415926535) ) , " (should be truncated)" );
		log( Notice() , "double pi x 10^15 : " , Real( double(3.1415926535e15) ) );
		log( Notice() , "float pi x 10^15 : " , Real( float(3.1415926535e15) ) , " (should be truncated)" );
		log( Notice() , "double pi x 10^-2 : " , Real( double(3.1415926535e-2) ) );
		log( Notice() , "float pi x 10^-2 : " , Real( float(3.1415926535e-2) ) , " (should be truncated)" );


		log( Notice() , "Boolean true : " , Boolean( true ) );
		log( Notice() , "Boolean false : " , Boolean( false ) );
		log( Notice() , "Boolean true : " , Boolean<alpha>( true ) );
		log( Notice() , "Boolean false : " , Boolean<alpha>( false ) );
		log( Notice() , "Boolean true : " , Boolean<numeric>( true ) );
		log( Notice() , "Boolean false : " , Boolean<numeric>( false ) );

		TestStruct<uint32_t> lTest = { 0x00112233 , 0x44556677 , 0x8899AABB , 0xCCDDEEFF };
		
		log( Notice() , "Hex dump (struct) : " , Hex( lTest ) );
		log( Notice() , "Hex dump (one of the logging wrappers) : " , Hex( Integer<hex , fixed>( uint64_t(13) ) ) );
		log( Notice() , "Hex dump (POD) : " , Hex( uint64_t(13) ) );


// Test setting logging levels
		log_configuration::setLogLevelTo( Error() ); 

		log( Notice() , std::string("IF YOU >>DO<< SEE THIS THEN THERE IS AN ERROR") );
		log( Error() , std::string("IF YOU DONT SEE THIS THEN THERE IS AN ERROR") );

		log_configuration::setLogLevelTo( Notice() ); 

		log( Notice() , std::string("IF YOU DONT SEE THIS THEN THERE IS AN ERROR") );
		log( Error() , std::string("IF YOU DONT SEE THIS THEN THERE IS AN ERROR") );


	}
	catch ( const std::exception& aExc )
	{
		std::cout << "Caught Exception : " << aExc.what() << std::endl;
	}
}


