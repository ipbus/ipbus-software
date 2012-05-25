//#define BOOST_TEST_DYN_LINK

//#define BOOST_TEST_MODULE uHAL Test Cases

//#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <vector>
#include <string>
#include <cstdlib>

#include "pantheios/frontends/fe.simple.h"

#include <sys/time.h>


/* Subtract the `struct timeval' values X and Y,
        storing the result in RESULT.
        Return 1 if the difference is negative, otherwise 0.  */
     
 int
 timeval_subtract (timeval *result, timeval *x, timeval *y)
 {
   /* Perform the carry for the later subtraction by updating y. */
   if (x->tv_usec < y->tv_usec) {
	 int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
	 y->tv_usec -= 1000000 * nsec;
	 y->tv_sec += nsec;
   }
   if (x->tv_usec - y->tv_usec > 1000000) {
	 int nsec = (x->tv_usec - y->tv_usec) / 1000000;
	 y->tv_usec += 1000000 * nsec;
	 y->tv_sec -= nsec;
   }
 
   /* Compute the time remaining to wait.
	  tv_usec is certainly positive. */
   result->tv_sec = x->tv_sec - y->tv_sec;
   result->tv_usec = x->tv_usec - y->tv_usec;
 
   /* Return 1 if result is negative. */
   return x->tv_sec < y->tv_sec;
 }

	 


int main ( int argc,char* argv[] )
{
	try
	{
		pantheios::log_LOCATION();
		uhal::ConnectionManager manager ( "file://tests/addr/connections.xml" );
		uhal::HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );

		uint32_t SIZE=10500;
		uint32_t ITERATIONS=10;

		std::vector< uint32_t > lData;
		for ( int i = 0 ; i != SIZE ; ++i ){
			lData.push_back( rand() );
		}
		
		
		hw.getClient()->writeBlock ( 0xBA5EADD4 , lData );
		hw.dispatch();
	
		uhal::ValVector< uint32_t > block2;
	
		timeval start, end , TimeTaken;
		gettimeofday ( &start, NULL );
		for ( int i = 0 ; i !=ITERATIONS ; ++i ){
			//uhal::ValVector< uint32_t > block2 = hw.getNode ( "TRANSMITTER.BRAM_DATA" ).readBlock ( SIZE );
			block2 = hw.getClient()->readBlock ( 0xBA5EADD4 , SIZE );
			
			hw.dispatch();
		}
		gettimeofday ( &end, NULL );
		
	
		timeval_subtract( &TimeTaken , &end , &start );
		
		std::cout << "Time elapsed " << TimeTaken.tv_sec << "s " << TimeTaken.tv_usec << "us" << std::endl;
		
		std::cout << "Rate " << ((double)SIZE * (double)ITERATIONS * 32.0)/( (1000000 * (double)TimeTaken.tv_sec) + (double)TimeTaken.tv_usec ) << "Mbit/s" << std::endl;
		
		
		std::vector< uint32_t >::const_iterator lSourceIt = lData.begin();
		uhal::ValVector< uint32_t >::const_iterator lReadIt = block2.begin();
		int count = 0;

		for ( ; lReadIt != block2.end() && lSourceIt != lData.end() ; ++lReadIt , ++lSourceIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				pantheios::log_ERROR ( "MISMATCH AT " , pantheios::integer ( count ) , " : Source " , pantheios::integer ( *lSourceIt , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer ( *lReadIt  ,  pantheios::fmt::fullHex | 10 ) );
				pantheios::log_LOCATION();
				throw 0;
			}
		}
	
		
		
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}
}


