/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_log_exception_hpp_
#define _uhal_log_exception_hpp_

#include <exception>
#include <string>
#include <sys/time.h>
#include <time.h>

#ifdef USE_BACKTRACE
#include "boost/thread.hpp"
#endif


/**
	Macro for simplifying the declaration and definition of derived exception types
*/
#define ExceptionClass( ClassName , ClassDescription )\
 class ClassName : public uhal::exception::exception {\
 public:\
 ClassName() : uhal::exception::exception() {}\
 void ThrowAsDerivedType_(){ throw ClassName(*this); } \
 exception* clone(){ return new ClassName(*this); } \
 protected:\
 std::string description() const throw() { return std::string( ClassDescription ); } \
};

/**
	Macro version of the member function to wrap the ThrowAsDerivedType but also tell the compiler that this function always throws
*/
#define ThrowAsDerivedType() ThrowAsDerivedType_(); throw 0;


#ifdef USE_BACKTRACE
#define MaxExceptionHistoryLength 100
#endif

namespace uhal
{

  //! A namespace for all exceptions to live in - this will hopefully make documentation a bit clearer
  namespace exception
  {

    //! An abstract base exception class providing an interface to a throw/ThrowAsDerivedType mechanism which will allow us to catch the base type and ThrowAsDerivedType the derived type
    class exception : public std::exception
    {
      public:
        /**
        	Constructor
        */
        exception ();

        /**
          Copy constructor
        		  @param aExc an exception to copy
        */
        exception ( const exception& aExc );

        /**
          Assignment operator
        		  @param aExc an exception to copy
        		  @return a reference to this object
        */
        exception& operator= ( const exception& aExc );

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
        			Function which casts a pointer from the base type of this object to a derived type of this object and rethrows as the derived type
        		*/
        virtual void ThrowAsDerivedType_() = 0;

        /**
        	Return a new object of the derived exception type cloned from this object
        	@return a new object of the derived exception type
        */
        virtual exception* clone() = 0;

        /**
        	Add additional information to the exception message
        	@param aCStr additional information to be added to the exception message
        */
        void append ( const char* aCStr ) throw();


      protected:
        /**
        	Return the description associated with this type of exception
        	@return the description associated with this type of exception
        */
        virtual std::string description() const throw() = 0;

      private:
#ifdef USE_BACKTRACE
        /**
        	List of pointers to the backtrace symbols
        */
        std::vector< void* > mBacktrace;

        /// The thread ID of the thread in which the exception was originally thrown
        boost::thread::id mThreadId;
#endif
        /// The time at which the exception was thrown
        timeval mTime;

        /// Memory which the call to "what()" uses when formatting the output string
        char* mString;

        /// Memory into which additional information is added by calls to append
        char* mAdditionalInfo;
    };

  }
}


#endif
