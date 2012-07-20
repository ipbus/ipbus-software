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

namespace uhal
{

	//! A simple exception class providing a little more functionality than std::exception
	class exception : public std::exception
	{
		protected:
			/**
				Constructor
				@param aExc a standard string to be used as a message
			*/
			exception ( const std::string& aExc = "" );

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
			virtual void throwFrom ( const Location& aLocation ) = 0;

			/**
				Abstract function to rethrow the current exception
				@param aLocation a location object which holds the function name, file name and line number where the rethrow occured
			*/
			virtual void rethrowFrom ( const Location& aLocation ) = 0;

		private:

			//! The message given to the exception at the time of construction
			std::string mMessage;

	};

	template < class Derived >
	class _exception : public exception
	{
		protected:
			/**
				Constructor
				@param aExc a standard string to be used as a message
			*/
			_exception ( const std::string& aExc = "" );

		public:
			/**
				Destructor
			*/
			virtual ~_exception() throw();

			/**
				Concrete implementation of function to throw the current exception
				@param aLocation a location object which holds the function name, file name and line number where the throw occured
			*/
			void throwFrom ( const Location& aLocation );

			/**
				Concrete implementation of function to rethrow the current exception
				@param aLocation a location object which holds the function name, file name and line number where the rethrow occured
			*/
			void rethrowFrom ( const Location& aLocation );
	};



	//! Exception class to handle the case where the supposedly unique ID is duplicated. Uses the base uhal::exception implementation of what()
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
