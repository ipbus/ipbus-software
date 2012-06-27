
#ifndef _log_backend_colour_terminal_hpp_
#define _log_backend_colour_terminal_hpp_

#include <stdio.h>
#include <stdint.h>

namespace uhal
{

	//! Function called by the log functions to add the line start to a log entry
	template< typename T >
	void log_head();

	//! Function called by the log functions to add the line ending to a log entry
	template< typename T >
	void log_tail();

	//! Helper function object to allow template specialization of the log_head function
	template< typename T >
	struct log_head_template_specialization_helper
	{
		private:
			//! Make log_head a friend since that is what this function object exists for...
			friend void log_head< T >();
			//! Static function to do the actual work
			static void print();
	};

	/**
		Function to copy a single character to the output buffer
		@param aChar a reference to a single character to copy to the output buffer
	*/
	void put ( const char& aChar );
	/**
		Function to copy a null-terminated character string to the output buffer
		@param aStr a pointer to the start of null-terminated character string to copy to the output buffer
	*/
	void put ( const char* aStr );
	/**
		Function to copy a number of characters from a specified location to the output buffer
		@param aStart the first character to copy to the output buffer
		@param aSize the number of characters to copy to the output buffer
	*/
	void put ( const char* aStart , const uint32_t& aSize );

}

#include <uhal/log/log_backend.colour_terminal.hxx>

#endif
