
#ifndef _log_inserters_time_hpp_
#define _log_inserters_time_hpp_

#include <log/log_inserter_helper.hpp> 

#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <stdint.h>

namespace uhal{


	enum time_element{
		null,
		year,
		yr,
		strmth,
		mth,
		day,
		hr,
		min,
		sec,
		usec
	};


	template< time_element T >
	struct TimeSpecializationHelper
	{
		static void print( FILE* aFile , const tm* aTm , const uint32_t& aUsec );
	};


	template< time_element T0 , char D0 = ' ' , 
				time_element T1 = null, char D1 = ' ' , 
				time_element T2 = null, char D2 = ' ' , 
				time_element T3 = null, char D3 = ' ' , 
				time_element T4 = null, char D4 = ' ' , 
				time_element T5 = null, char D5 = ' ' , 
				time_element T6 = null >
	class Time : public RefWrapper< timeval >
	{
		public:	
			Time( const timeval& aTimeval ) : RefWrapper< timeval >( aTimeval ){}
	};

}

#include <log/log_inserters.time.hxx>

#endif
