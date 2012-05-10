/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_exception_hpp_
#define _uhal_exception_hpp_

#include <exception>
#include <string>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

	//! A simple exception class providing a little more functionality than std::exception
	class exception : public std::exception
	{
		public:
			/**
				Default constructor
			*/
			exception() :
				std::exception() ,
				mMessage ( "" )
			{}

			/**
				Constructor
			*/
			exception ( const std::exception& aExc ) :
				std::exception ( aExc ) ,
				mMessage ( aExc.what() )
			{}

			/**
				Destructor
			*/
			virtual ~exception() throw() {}

			/**
				Function which returns the error message associated with an exception
				If no error message has previously been defined, then it makes the typename readable (where appropriate) and returns this instead.
			*/
			virtual const char* what() const throw()
			{
				if ( mMessage.size() )
				{
					return mMessage.c_str();
				}

#ifdef __GNUG__
				// this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
				int lStatus ( 0 );
				return abi::__cxa_demangle ( typeid ( *this ).name() , 0 , 0 , &lStatus );
#endif
				return typeid ( *this ).name();
			}
		private:

			//! The message given to the exception at the time of construction
			std::string mMessage;

	};

}


#endif
