//#define BOOST_TEST_DYN_LINK

//#define BOOST_TEST_MODULE uHAL Test Cases

//#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <vector>
#include <string>
#include <cstdlib>

#include "pantheios/frontends/fe.simple.h"

#include <boost/lexical_cast.hpp>
#include <sys/time.h>



int main ( int argc,char* argv[] )
{
	try
	{
		pantheios_fe_simple_setSeverityCeiling ( pantheios::debug );
				uhal::ConnectionManager manager ( "file://tests/addr/connections.xml" );
		uhal::HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
		uint32_t SIZE=10500;
		uint32_t ITERATIONS=10000;
		std::vector< uint32_t > lData;

		for ( int i = 0 ; i != SIZE ; ++i )
		{
			lData.push_back ( rand() );
		}

		hw.getClient()->writeBlock ( 0xBA5EADD4 , lData );
		hw.dispatch();

// 		std::string lString( boost::lexical_cast< std::string >( ITERATIONS-1 ) );
// 		uint32_t lLength( lString.size() );
// 		std::cout << "ITERATION " << std::string( lLength , ' ' ) << "/" << lString << std::string( lLength+1 , '\b' ) << std::flush;
		char lChar[] = { '-' , '/' , '|' , '\\' }; 
		std::cout << "Progress :  " << std::flush;

		uhal::ValVector< uint32_t > block2;
		timeval lStart, lEnd;
		gettimeofday ( &lStart, NULL );

		for ( int i = 0 ; i !=ITERATIONS ; ++i )
		{
// 			std::cout << std::string( lLength , '\b' ) << std::setw( lLength ) << i << std::flush;
			std::cout << '\b' << lChar[i & 0x03] << std::flush;

			//uhal::ValVector< uint32_t > block2 = hw.getNode ( "TRANSMITTER.BRAM_DATA" ).readBlock ( SIZE );
			block2 = hw.getClient()->readBlock ( 0xBA5EADD4 , SIZE );
			hw.dispatch();
		}

		gettimeofday ( &lEnd, NULL );
		
		double lStart2( ( ( double ) lStart.tv_sec*1e6 ) + ( ( double ) lStart.tv_usec ) ); 
		double lEnd2( ( ( double ) lEnd.tv_sec*1e6 ) + ( ( double ) lEnd.tv_usec ) );
		double lTimeTaken ( lEnd2 - lStart2 );
		
		std::cout << "\n\nTime elapsed " << (lTimeTaken/1e6) << "s" << std::endl;
		std::cout << "Rate " << ( ( double ) SIZE * ( double ) ITERATIONS * 32.0 ) / lTimeTaken << "Mbit/s" << std::endl;


		std::vector< uint32_t >::const_iterator lSourceIt = lData.begin();
		uhal::ValVector< uint32_t >::const_iterator lReadIt = block2.begin();
		int count = 0;

		for ( ; lReadIt != block2.end() && lSourceIt != lData.end() ; ++lReadIt , ++lSourceIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				pantheios::log_ERROR ( "MISMATCH AT " , pantheios::integer ( count ) , 
										" : Source " , pantheios::integer ( *lSourceIt , pantheios::fmt::fullHex | 10 ) , 
										" vs. Found " , pantheios::integer ( *lReadIt  ,  pantheios::fmt::fullHex | 10 ) );
								// throw 0;
			}
		}

		std::cout << "All came back good" << std::endl;

	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}
}


