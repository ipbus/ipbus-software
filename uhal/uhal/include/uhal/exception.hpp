/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_exception_hpp_
#define _uhal_exception_hpp_

#include <exception>
#include <string>

namespace uhal
{

	//! A simple exception class providing a little more functionality than std::exception
	class exception : public std::exception
	{
		public:
			/**
				Default constructor
			*/
			exception();

			/**
				Destructor
			*/
			virtual ~exception() throw();
			
			/**
				Function which returns the error message associated with an exception
				If no error message has previously been defined, then it makes the typename readable (where appropriate) and returns this instead.
				@return the error message associated with an exception
			*/
			virtual const char* what() const throw();

			// Doxygen does something very, very odd if this is placed above any of the other functions...
			/**
				Constructor
				@param aExc an exception whose message is to be copied
			*/
			exception ( const std::exception& aExc );
			
		private:

			//! The message given to the exception at the time of construction
			std::string mMessage;

	};

}


#endif
