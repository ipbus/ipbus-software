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
      std::exception ()
    {}

    exception::~exception() throw() {}

    const char* exception::what() const throw()
    {
      for ( map_type::iterator lIt = mExceptions.begin(); lIt != mExceptions.end(); ++lIt )
      {
        mMessage += "Thread ";
        mMessage += boost::lexical_cast<std::string> ( lIt->first );
        mMessage += ":\n";

        uint32_t i(0);
        for ( std::list<exception_helper>::iterator lIt2 = lIt->second.begin() ; lIt2 != lIt->second.end() ; ++ lIt2, ++i )
        {
          mMessage += " - Exception ";
          mMessage += boost::lexical_cast<std::string> ( i );
          mMessage += ":\n";

          mMessage += lIt2->mMessages;
        }
      }

      return mMessage.c_str(); //result from c_str is valid for as long as the object exists or until it is modified after the c_str operation.
    }

    void exception::create() throw()
    {
      exception_helper lExcHlpr ( this );
      mExceptions[ boost::this_thread::get_id() ].push_back ( lExcHlpr );
    }


    std::string exception::mMessage = std::string();
    exception::map_type exception::mExceptions = exception::map_type();
  }
}


namespace uhal
{
  namespace exception
  {

    exception_helper::exception_helper ( exception* aExc ) :
      mExc ( aExc )
    {
      mMessages += "   - Exception of type: ";
#ifdef __GNUG__
      // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
      int lStatus ( 0 );
      mMessages += abi::__cxa_demangle ( typeid ( *mExc ).name() , 0 , 0 , &lStatus );
#else
      mMessages += typeid ( *mExc ).name();
#endif
      mMessages += '\n';
      mMessages += "   - Description: ";
      mMessages += mExc->description();
      mMessages += '\n';
    }

    exception_helper::~exception_helper()
    {}

  }
}

