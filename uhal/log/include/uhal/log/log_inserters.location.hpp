
#ifndef _log_inserters_location_hpp_
#define _log_inserters_location_hpp_

#define ThisLocation() uhal::Location( __PRETTY_FUNCTION__ , __FILE__ , __LINE__ )

#include <stdint.h>

namespace uhal
{

	//! A class to wrap the function name, filename and line-number location of its construction for the purpose of debugging and tracking unwinding exceptions
	class Location
	{

		public:
			/**
				Constructor
				@param aFunction the name of the current function as returned by the __PRETTY_FUNCTION__ macro
				@param aFile the name of the current file as returned by the __FILE__ macro
				@param aLine the number of the current line as returned by the __LINE__ macro
			*/
			Location ( const char* aFunction , const char* aFile , const uint32_t& aLine );

			//! the name of the current function as returned by the __PRETTY_FUNCTION__ macro
			const char* mFunction;
			//! the name of the current file as returned by the __FILE__ macro
			const char* mFile;
			//! the number of the current line as returned by the __LINE__ macro
			const uint32_t mLine;
	};


}

#endif
