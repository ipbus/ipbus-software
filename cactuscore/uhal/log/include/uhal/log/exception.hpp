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
#include <map>
#include <list>
#include <sys/time.h>
#include <time.h>

#include "uhal/log/log_inserters.location.hpp"
#include "boost/thread.hpp"


#define ExceptionClass( ClassName , ClassDescription )\
 class ClassName : public uhal::exception::exception {\
 public:\
 ClassName() : uhal::exception::exception() {}\
 void ThrowAsDerivedType_(){ throw ClassName(*this); } \
 exception* clone(){ return new ClassName(*this); } \
 protected:\
 std::string description() const throw() { return std::string( ClassDescription ); } \
};

#define ThrowAsDerivedType() ThrowAsDerivedType_(); throw 0;

#define MaxExceptionHistoryLength 100


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
        	Destructor
        */
        virtual ~exception() throw();

        /**
        	Function which returns the error message associated with an exception
        	If no error message has previously been defined, then it makes the typename readable (where appropriate) and returns this instead.
        	@return the error message associated with an exception
        */
        virtual const char* what() const throw();

        virtual void ThrowAsDerivedType_() = 0;

        virtual exception* clone() = 0;

        virtual void append ( const std::string& aMessage ) throw();

      protected:
        virtual std::string description() const throw() = 0;

      private:
        std::vector< std::string > mAdditionalInfo;
        std::vector< void* > mBacktrace;
        boost::thread::id mThreadId;
        timeval mTime;

      private:
        static std::string mMemory;
    };

  }
}


#endif
