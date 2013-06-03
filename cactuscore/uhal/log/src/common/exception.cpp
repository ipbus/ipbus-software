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

#include "uhal/log/BacktraceSymbols.hpp"

#include "uhal/log/exception.hpp"
#include "boost/lexical_cast.hpp"

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{
  namespace exception
  {

    exception::exception ( ) :
      std::exception (),
      mBacktrace ( MaxExceptionHistoryLength , static_cast<void*> ( NULL ) ),
      mThreadId ( boost::this_thread::get_id() )
    {
      Backtrace ( mBacktrace );
    }

    exception::~exception() throw()
    {}


    const char* exception::what() const throw()
    {
      std::stringstream lStr;
      lStr << "\n";
#ifdef COURTEOUS_EXCEPTIONS
      lStr << "I'm terribly sorry to have to tell you this, but it appears that there was an exception:\n";
#endif
      lStr << " * Exception type: ";
#ifdef __GNUG__
      // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
      int lStatus ( 0 );
      lStr << abi::__cxa_demangle ( typeid ( *this ).name() , 0 , 0 , &lStatus );
#else
      lStr << typeid ( *this ).name();
#endif
      lStr << "\n";
      lStr << " * Description: " << description() << "\n";

      if ( mAdditionalInfo.size() )
      {
        lStr << " * Additional Information:\n";

        for ( std::vector< std::string >::const_iterator lIt = mAdditionalInfo.begin() ; lIt != mAdditionalInfo.end(); ++lIt )
        {
          lStr << "   - " << *lIt << "\n";
        }
      }

      boost::thread::id lThreadId ( boost::this_thread::get_id() );

      if ( mThreadId == lThreadId )
      {
        lStr << " * Exception occured in the same thread as that in which it was caught (" << lThreadId << ")\n";
      }
      else
      {
        lStr << " * Exception occured in thread with ID: " << mThreadId << "\n";
        lStr << " * Current thread has ID: " << lThreadId << "\n";
      }

      lStr << " * Call stack:\n";
      std::vector< TracePoint > lBacktrace = BacktraceSymbols ( mBacktrace );
      uint32_t lCounter ( 0 );

      for ( std::vector< TracePoint >::iterator lIt = lBacktrace.begin() +3 ; lIt != lBacktrace.end(); ++lIt , ++lCounter )
      {
        lStr << "   [ " << lCounter << " ] " << lIt->function << "\n";
        lStr << "            at " << lIt->file << ":" << lIt->line << "\n";
      }

      mMemory = lStr.str();
      return mMemory.c_str(); //result from c_str is valid for as long as the object exists or until it is modified after the c_str operation.
    }


    void exception::append ( const std::string& aMessage ) throw()
    {
      mAdditionalInfo.push_back ( aMessage );
    }

    std::string exception::mMemory; //result from c_str is valid for as long as the object exists or until it is modified after the c_str operation.

  }
}



