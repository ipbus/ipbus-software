/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

---------------------------------------------------------------------------
*/

/**
	@file
	@author Andrew W. Rose
	@date 2012
*/


#include <iostream>

#include "uhal/log/log.hpp"

#include "uhal/utilities/files.hpp"


using namespace uhal;

template< typename T >
struct TestStruct
{
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
    log ( Notice() , "const char*" );
    log ( Notice() , std::string ( "std::string" ) );
    log ( Notice() , 'c' );
    log ( Notice() , "a" , 'b' , std::string ( "c" ) , "d" , "e" , "f" , "g" , "h" , "i" , "j" , "k" , "l" , "m" , "n" , "o" , "p" , "q" , "r" , "s" , "t" , "u" , "v" , "w" , "x" , "y" , "z" , "0" , "1" , "2" , "3" , "4" , "5" );
    timeval lTime;
    gettimeofday ( &lTime, NULL );
    log ( Notice() , Time ( lTime ) , " default formatting" );
    log ( Notice() , Time ( lTime , TimeFmt< day,'/',mth,'/',year,' ',hr,':',min,':',sec,'.',usec >() ) , " truncated to the nearest usecond" );
    log ( Notice() , Time ( lTime , TimeFmt< day,'/',mth,'/',year,' ',hr,':',min,':',sec >() ) , " truncated to the nearest second" );
    log ( Notice() , Time ( lTime , TimeFmt< day,'/',mth,'/',year,' ',hr,':',min >() ) , " truncated to the nearest minute" );
    log ( Notice() , Time ( lTime , TimeFmt< day,'/',mth,'/',year,' ',hr >() ) , " truncated to the nearest hour" );
    log ( Notice() , Time ( lTime , TimeFmt< day,'/',mth,'/',year >() ) , " truncated to the nearest day" );
    log ( Notice() , Time ( lTime , TimeFmt< year,'/',mth,'/',day >() ) , " truncated to the nearest day (US format)" );
    log ( Notice() , Time ( lTime , TimeFmt< mth,'/',year >() ) , " truncated to the nearest month" );
    log ( Notice() , Time ( lTime , TimeFmt< strmth,' ',year >() ) , " truncated to the nearest month" );
    log ( Notice() , Time ( lTime , TimeFmt< year >() ) , " truncated to the nearest year (4 digit format)" );
    log ( Notice() , Time ( lTime , TimeFmt< yr >() ) , " truncated to the nearest year (2 digit format)" );
    log ( Notice() , Time ( lTime , TimeFmt< usec >() ) , " microseconds" );
    log ( Notice() , "13 as a decimal uint8 : " , Integer ( uint8_t ( 13 ) ) );
    log ( Notice() , "-13 as a decimal int8 : " , Integer ( int8_t ( -13 ) ) );
    log ( Notice() , "13 as a decimal uint16 : " , Integer ( uint16_t ( 13 ) ) );
    log ( Notice() , "-13 as a decimal int16 : " , Integer ( int16_t ( -13 ) ) );
    log ( Notice() , "13 as a decimal uint32 : " , Integer ( uint32_t ( 13 ) ) );
    log ( Notice() , "-13 as a decimal int32 : " , Integer ( int32_t ( -13 ) ) );
    log ( Notice() , "13 as a decimal uint64 : " , Integer ( uint64_t ( 13 ) ) );
    log ( Notice() , "-13 as a decimal int64 : " , Integer ( int64_t ( -13 ) ) );
    log ( Notice() , "13 as a variable-width binary uint8 : " , Integer ( uint8_t ( 13 ) , IntFmt<bin>() ) );
    log ( Notice() , "-13 as a variable-width binary int8 : " , Integer ( int8_t ( -13 ) , IntFmt<bin>() ) );
    log ( Notice() , "13 as a variable-width binary uint16 : " , Integer ( uint16_t ( 13 ) , IntFmt<bin>() ) );
    log ( Notice() , "-13 as a variable-width binary int16 : " , Integer ( int16_t ( -13 ) , IntFmt<bin>() ) );
    log ( Notice() , "13 as a variable-width binary uint32 : " , Integer ( uint32_t ( 13 ) , IntFmt<bin>() ) );
    log ( Notice() , "-13 as a variable-width binary int32 : " , Integer ( int32_t ( -13 ) , IntFmt<bin>() ) );
    log ( Notice() , "13 as a variable-width binary uint64 : " , Integer ( uint64_t ( 13 ) , IntFmt<bin>() ) );
    log ( Notice() , "-13 as a variable-width binary int64 : " , Integer ( int64_t ( -13 ) , IntFmt<bin>() ) );
    log ( Notice() , "13 as a variable-width hex uint8 : " , Integer ( uint8_t ( 13 ) , IntFmt<hex>() ) );
    log ( Notice() , "-13 as a variable-width hex int8 : " , Integer ( int8_t ( -13 ) , IntFmt<hex>() ) );
    log ( Notice() , "13 as a variable-width hex uint16 : " , Integer ( uint16_t ( 13 ) , IntFmt<hex>() ) );
    log ( Notice() , "-13 as a variable-width hex int16 : " , Integer ( int16_t ( -13 ) , IntFmt<hex>() ) );
    log ( Notice() , "13 as a variable-width hex uint32 : " , Integer ( uint32_t ( 13 ) , IntFmt<hex>() ) );
    log ( Notice() , "-13 as a variable-width hex int32 : " , Integer ( int32_t ( -13 ) , IntFmt<hex>() ) );
    log ( Notice() , "13 as a variable-width hex uint64 : " , Integer ( uint64_t ( 13 ) , IntFmt<hex>() ) );
    log ( Notice() , "-13 as a variable-width hex int64 : " , Integer ( int64_t ( -13 ) , IntFmt<hex>() ) );
    log ( Notice() , "13 as a fixed-width binary uint8 : " , Integer ( uint8_t ( 13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width binary int8 : " , Integer ( int8_t ( -13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "13 as a fixed-width binary uint16 : " , Integer ( uint16_t ( 13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width binary int16 : " , Integer ( int16_t ( -13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "13 as a fixed-width binary uint32 : " , Integer ( uint32_t ( 13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width binary int32 : " , Integer ( int32_t ( -13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "13 as a fixed-width binary uint64 : " , Integer ( uint64_t ( 13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width binary int64 : " , Integer ( int64_t ( -13 ) , IntFmt<bin , fixed>() ) );
    log ( Notice() , "13 as a fixed-width hex uint8 : " , Integer ( uint8_t ( 13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width hex int8 : " , Integer ( int8_t ( -13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "13 as a fixed-width hex uint16 : " , Integer ( uint16_t ( 13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width hex int16 : " , Integer ( int16_t ( -13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "13 as a fixed-width hex uint32 : " , Integer ( uint32_t ( 13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width hex int32 : " , Integer ( int32_t ( -13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "13 as a fixed-width hex uint64 : " , Integer ( uint64_t ( 13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "-13 as a fixed-width hex int64 : " , Integer ( int64_t ( -13 ) , IntFmt<hex , fixed>() ) );
    log ( Notice() , "Specified fixed width bin uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<bin , fixed , 10>() ) );
    log ( Notice() , "Specified fixed width dec uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<dec , fixed , 10>() ) );
    log ( Notice() , "Specified fixed width hex uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<hex , fixed , 10>() ) );
    //     log ( Notice() , "double pi : " , Real ( double ( 3.1415926535 ) ) );
    //     log ( Notice() , "float pi : " , Real ( float ( 3.1415926535 ) ) , " (should be truncated)" );
    //     log ( Notice() , "double pi : " , Real ( double ( 3.1415926535 ) , RealFmt<4>() ) );
    //     log ( Notice() , "float pi : " , Real ( float ( 3.1415926535 ) , RealFmt<4>() ) , " (should be truncated)" );
    //     log ( Notice() , "double pi x 10^15 : " , Real ( double ( 3.1415926535e15 ) ) );
    //     log ( Notice() , "float pi x 10^15 : " , Real ( float ( 3.1415926535e15 ) ) , " (should be truncated)" );
    //     log ( Notice() , "double pi x 10^-2 : " , Real ( double ( 3.1415926535e-2 ) ) );
    //     log ( Notice() , "float pi x 10^-2 : " , Real ( float ( 3.1415926535e-2 ) ) , " (should be truncated)" );
    //     log ( Notice() , "Boolean true : " , Boolean ( true ) );
    //     log ( Notice() , "Boolean false : " , Boolean ( false ) );
    //     log ( Notice() , "Boolean true : " , Boolean ( true , BoolFmt<alpha>() ) );
    //     log ( Notice() , "Boolean false : " , Boolean ( false , BoolFmt<alpha>() ) );
    //     log ( Notice() , "Boolean true : " , Boolean ( true , BoolFmt<numeric>() ) );
    //     log ( Notice() , "Boolean false : " , Boolean ( false , BoolFmt<numeric>() ) );
    //     TestStruct<uint32_t> lTest = { 0x00112233 , 0x44556677 , 0x8899AABB , 0xCCDDEEFF };
    //     log ( Notice() , "Hex dump (struct) : " , Hex ( lTest ) );
    //     log ( Notice() , "Hex dump (one of the logging wrappers) : " , Hex ( Integer ( uint64_t ( 13 ) ) ) );
    //     log ( Notice() , "Hex dump (POD) : " , Hex ( uint64_t ( 13 ) ) );
    //     log ( Notice() , "Pointer : " , Pointer ( &lTest ) );
    //     log ( Notice() , "Pointer : " , Pointer ( argv ) );
    //     log ( Notice() , "Pointer : " , Pointer ( *argv ) );
    log ( Notice() , "Quote : " , Quote ( Type< TestStruct<uint32_t> >() ) );
    log ( Notice() , "Specified fixed width hex uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<hex , fixed , 10>() ) );
    exception::FileNotFound lExc;
    log ( lExc , "Specified fixed width hex uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<hex , fixed , 10>() ) );
    log ( lExc , "Specified fixed width hex uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<hex , fixed , 10>() ) );
    log ( lExc , "Specified fixed width hex uint8_t : " , Integer ( uint8_t ( 13 ) , IntFmt<hex , fixed , 10>() ) );
    std::cout << lExc.what() << std::endl;
    // Test setting logging levels
    setLogLevelTo ( Error() );
    log ( Notice() , std::string ( "setLogLevelTo ( Error() ) : IF YOU >>DO<< SEE THIS, THEN THERE IS AN ERROR" ) );
    log ( Error() , std::string ( "setLogLevelTo ( Error() ) : IF YOU DON'T SEE THIS, THEN THERE IS AN ERROR" ) );
    setLogLevelTo ( Notice() );
    log ( Notice() , std::string ( "setLogLevelTo ( Notice() ) : IF YOU DON'T SEE THIS, THEN THERE IS AN ERROR" ) );
    log ( Error() , std::string ( "setLogLevelTo ( Notice() ) : IF YOU DON'T SEE THIS, THEN THERE IS AN ERROR" ) );
    //		log ( Notice() , "Dodgy cast : " , Integer ( bool(true) , IntFmt<hex , fixed>() ) );
    //		log ( Notice() , "Dodgy cast : " , Boolean ( uint32_t(13) ) );
    setLogLevelFromEnvironment ( "UHAL_LOG_LEVEL" );
    log ( Notice() , std::string ( "setLogLevelFromEnvironment ( \"UHAL_LOG_LEVEL\" ) : IF \"UHAL_LOG_LEVEL\" IS DEFINED ABOVE \"Notice()\" AND YOU >>DO<< SEE THIS, THEN THERE IS AN ERROR" ) );
    log ( Error() , std::string ( "setLogLevelFromEnvironment ( \"UHAL_LOG_LEVEL\" ) : IF \"UHAL_LOG_LEVEL\" IS NOT DEFINED ABOVE \"Error()\" AND YOU DON'T SEE THIS, THEN THERE IS AN ERROR" ) );
    disableLogging();
    log ( Notice() , std::string ( "disableLogging() : IF YOU >>DO<< SEE THIS, THEN THERE IS AN ERROR" ) );
    log ( Error() , std::string ( "disableLogging() : IF YOU >>DO<< SEE THIS, THEN THERE IS AN ERROR" ) );
    //Set log back to notice
    setLogLevelTo ( Notice() );
    log ( Notice() , std::string ( "setLogLevelTo ( Notice() ) : IF YOU DON'T SEE THIS, THEN THERE IS AN ERROR" ) );
    log ( Error() , std::string ( "setLogLevelTo ( Notice() ) : IF YOU DON'T SEE THIS, THEN THERE IS AN ERROR" ) );
  }
  catch ( const std::exception& aExc )
  {
    std::cerr << "ERROR: Caught Exception : " << aExc.what() << std::endl;
    exit ( 1 );
  }
}


