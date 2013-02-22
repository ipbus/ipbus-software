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
#include "uhal/log/log_inserters.location.hpp"
#include "boost/thread.hpp"


#define ExceptionClass( ClassName , ClassDescription )\
 class ClassName : public uhal::exception::exception {\
 public:\
 ClassName() : exception() { create(); }\
 protected:\
 std::string description() const throw() { return std::string( ClassDescription ); } \
};

namespace uhal
{

  //! A namespace for all exceptions to live in - this will hopefully make documentation a bit clearer
  namespace exception
  {

    class exception;

    class exception_helper
    {
        friend class exception;

      private:
        exception_helper ( exception* aExc );

      public:
        ~exception_helper();

      private:
        exception* mExc;
        std::string mMessages;
    };

    //! An abstract base exception class providing an interface to a throw/rethrow mechanism which will allow us to catch the base type and rethrow the derived type
    class exception : public std::exception
    {
        friend class exception_helper;

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

      protected:
        virtual std::string description() const throw() = 0;


        void create() throw();

      private:
        typedef std::map< boost::thread::id , std::list<exception_helper> > map_type;

        static std::string mMessage;
        static map_type mExceptions;

        //!Define a static Mutex lock for thread safe logging
        static boost::mutex mMutex;
    };




  }
}


#endif
