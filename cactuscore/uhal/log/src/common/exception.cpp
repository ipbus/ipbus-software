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

#ifdef USE_BACKTRACE
#include "uhal/log/BacktraceSymbols.hpp"
#include "uhal/log/GccOutputCleaner.hpp"
#endif

#include "uhal/log/exception.hpp"
#include "boost/lexical_cast.hpp"
#include <iostream>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{
  namespace exception
  {

    exception::exception ( ) :
      std::exception (),
#ifdef USE_BACKTRACE
      mBacktrace ( MaxExceptionHistoryLength , static_cast<void*> ( NULL ) ),
      mThreadId ( boost::this_thread::get_id() ),
#endif
      mString ( ( char* ) malloc ( 65536 ) ),
      mAdditionalInfo ( ( char* ) malloc ( 65536 ) )
    {
      gettimeofday ( &mTime, NULL );
#ifdef USE_BACKTRACE
      Backtrace::Backtrace ( mBacktrace );
#endif
      mAdditionalInfo[0] = '\0'; //malloc is not required to initialize to null, so do it manually, just in case
    }


    exception::exception ( const exception& aExc ) :
      std::exception (),
#ifdef USE_BACKTRACE
      mBacktrace ( aExc.mBacktrace ),
      mThreadId ( aExc.mThreadId ),
#endif
      mTime ( aExc.mTime ),
      mString ( ( char* ) malloc ( 65536 ) ),
      mAdditionalInfo ( ( char* ) malloc ( 65536 ) )
    {
      strcpy ( mAdditionalInfo , aExc.mAdditionalInfo );
    }

    exception& exception::operator= ( const exception& aExc )
    {
      strcpy ( mAdditionalInfo , aExc.mAdditionalInfo );
#ifdef USE_BACKTRACE
      mBacktrace = aExc.mBacktrace;
      mThreadId = aExc.mThreadId;
#endif
      mTime = aExc.mTime;
      return *this;
    }

    exception::~exception() throw()
    {
      if ( mString )
      {
        free ( mString );
        mString = NULL;
      }

      if ( mAdditionalInfo )
      {
        free ( mAdditionalInfo );
        mAdditionalInfo = NULL;
      }
    }


    const char* exception::what() const throw()
    {
      if ( mString == NULL )
      {
        std::cout << "Could not allocate memory for exception message" << std::endl;
        return mString;
      }

      std::stringstream lStr;

#ifdef USE_BACKTRACE

      timeval lTime;
      gettimeofday ( &lTime, NULL );
      lStr << "\n";
#ifdef COURTEOUS_EXCEPTIONS
      lStr << "I'm terribly sorry to have to tell you this, but it appears that there was an exception:\n";
#endif
      lStr << " * Exception type: ";
#ifdef __GNUG__
      // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
      int lStatus ( 0 );
      std::size_t lSize ( 1024 );
      char lDemangled[lSize];
      lStr << abi::__cxa_demangle ( typeid ( *this ).name() , lDemangled , &lSize , &lStatus );
#else
      lStr << typeid ( *this ).name();
#endif
      lStr << "\n";
      lStr << " * Description: " << description() << "\n";

      if ( strlen ( mAdditionalInfo ) )
      {
        lStr << " * Additional Information:\n";
        lStr << mAdditionalInfo;
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

      lStr << std::setfill ( '0' );
      char tmbuf[64];
      strftime ( tmbuf, sizeof tmbuf, "%Y-%m-%d %H:%M:%S", localtime ( &mTime.tv_sec ) );
      lStr << " * Exception constructed at time:              " << tmbuf << '.' << std::setw ( 6 ) << mTime.tv_usec << "\n";
      strftime ( tmbuf, sizeof tmbuf, "%Y-%m-%d %H:%M:%S", localtime ( &lTime.tv_sec ) );
      lStr << " * Exception's what() function called at time: " << tmbuf << '.' << std::setw ( 6 ) << lTime.tv_usec << "\n";

      lStr << " * Call stack:\n";
      std::vector< Backtrace::TracePoint > lBacktrace = Backtrace::BacktraceSymbols ( mBacktrace );
      uint32_t lCounter ( 0 );
      GccOutputCleaner lOutputCleaner ( 12 , &GccOutputCleaner::TStyle );

      for ( std::vector< Backtrace::TracePoint >::iterator lIt = lBacktrace.begin() +3 ; lIt != lBacktrace.end(); ++lIt , ++lCounter )
      {
        lStr << "   [ " << std::setw ( 2 ) << lCounter << " ] " << lOutputCleaner ( lIt->function ) << "\n";
        lStr << "          at " << lIt->file << ":" << lIt->line << "\n";
      }

#else

      if ( strlen ( mAdditionalInfo ) )
      {
        lStr << mAdditionalInfo;
      }
      else
      {
        lStr << description() << " (no additional info)";
      }

#endif

      std::string lString ( lStr.str() );
      strncpy ( mString , lString.c_str() , 65536 );

      if ( lString.size() > 65536 )
      {
        strcpy ( mString+65530 , "..." );
      }

      return mString;
    }


    void exception::append ( const char* aCStr ) throw()
    {
      strncat ( mAdditionalInfo, aCStr , 65536-strlen ( mAdditionalInfo ) );
    }

  }
}



