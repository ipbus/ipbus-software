/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_exception_hpp_
#define _uhal_exception_hpp_

#include <exception>
#include <string>

#include "uhal/log/log.hpp"

//! An alias for the member "throwFrom" function which allows us to reassure the compiler that the catch block is actually going to rethrow...
#define throwFrom(LOC) throwFrom_ ( LOC ); throw 0;
//! An alias for the member "rethrowFrom" function which allows us to reassure the compiler that the catch block is actually going to rethrow...
#define rethrowFrom(LOC) rethrowFrom_ ( LOC ); throw 0;

namespace uhal
{

	//! An abstract base exception class providing an interface to a throw/rethrow mechanism which will allow us to catch the base type and rethrow the derived type
	class exception : public std::exception
	{
		public:
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

			/**
				Abstract function to throw the current exception
				@param aLocation a location object which holds the function name, file name and line number where the throw occured
			*/
			virtual void throwFrom_ ( const Location& aLocation ) = 0;

			/**
				Abstract function to rethrow the current exception
				@param aLocation a location object which holds the function name, file name and line number where the rethrow occured
			*/
			virtual void rethrowFrom_ ( const Location& aLocation ) = 0;

		protected:
			/**
				Constructor
				@param aExc a standard string to be used as a message
			*/
			exception ( const std::string& aExc = "" );
			
		private:

			//! The message given to the exception at the time of construction
			std::string mMessage;

	};

	//! A concrete exception class using CRTP to provide an implementation for the throw/rethrow mechanism, allowing us to catch the base type and rethrow the derived type
	template < class Derived >
	class _exception : public exception
	{
		public:
			/**
				Destructor
			*/
			virtual ~_exception() throw();

			/**
				Concrete implementation of function to throw the current exception
				@param aLocation a location object which holds the function name, file name and line number where the throw occured
			*/
			void throwFrom_ ( const Location& aLocation );

			/**
				Concrete implementation of function to rethrow the current exception
				@param aLocation a location object which holds the function name, file name and line number where the rethrow occured
			*/
			void rethrowFrom_ ( const Location& aLocation );
			
		protected:
			/**
				Constructor
				@param aExc a standard string to be used as a message
			*/
			_exception ( const std::string& aExc = "" );			
	};



	//! Exception class to handle the case where we catch a std::exception and want to throw it as a uhal::exception. Uses the base uhal::exception implementation of what()
	class StdException: public uhal::_exception< StdException >
	{
		public:
			/**
				Constructor
				@param aExc a standard exception whose message is to be copied
			*/
			StdException ( const std::exception& aExc );
	};


}


#include "uhal/TemplateDefinitions/exception.hxx"

#endif
