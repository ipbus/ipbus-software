//#define BOOST_TEST_DYN_LINK

//#define BOOST_TEST_MODULE uHAL Test Cases

//#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <vector>
#include <string>
#include <cstdlib>

#include <boost/lexical_cast.hpp>
#include <sys/time.h>

#include "uhal/log/log.hpp"

using namespace uhal;

int main ( int argc,char* argv[] )
{
	try
	{
		setLogLevelTo ( Debug() );
		ConnectionManager manager ( "file://tests/addr/connections.xml" );
		HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
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
		ValVector< uint32_t > block2;
		timeval lStart, lEnd;
		gettimeofday ( &lStart, NULL );

		for ( int i = 0 ; i !=ITERATIONS ; ++i )
		{
			// 			std::cout << std::string( lLength , '\b' ) << std::setw( lLength ) << i << std::flush;
			std::cout << '\b' << lChar[i & 0x03] << std::flush;
			//ValVector< uint32_t > block2 = hw.getNode ( "TRANSMITTER.BRAM_DATA" ).readBlock ( SIZE );
			block2 = hw.getClient()->readBlock ( 0xBA5EADD4 , SIZE );
			hw.dispatch();
		}

		gettimeofday ( &lEnd, NULL );
		double lStart2 ( ( ( double ) lStart.tv_sec*1e6 ) + ( ( double ) lStart.tv_usec ) );
		double lEnd2 ( ( ( double ) lEnd.tv_sec*1e6 ) + ( ( double ) lEnd.tv_usec ) );
		double lTimeTaken ( lEnd2 - lStart2 );
		std::cout << "\n\nTime elapsed " << ( lTimeTaken/1e6 ) << "s" << std::endl;
		std::cout << "Rate " << ( ( double ) SIZE * ( double ) ITERATIONS * 32.0 ) / lTimeTaken << "Mbit/s" << std::endl;
		std::vector< uint32_t >::const_iterator lSourceIt = lData.begin();
		ValVector< uint32_t >::const_iterator lReadIt = block2.begin();
		int count = 0;

		for ( ; lReadIt != block2.end() && lSourceIt != lData.end() ; ++lReadIt , ++lSourceIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				log ( Error() , "MISMATCH AT " , Integer ( count ) ,
					  " : Source " , Integer ( *lSourceIt , IntFmt< hex , fixed >() ) ,
					  " vs. Found " , Integer ( *lReadIt , IntFmt< hex , fixed >() ) );
				// throw 0;
			}
		}

		std::cout << "All came back good" << std::endl;
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception " , Quote( aExc.what() ) , " caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}


