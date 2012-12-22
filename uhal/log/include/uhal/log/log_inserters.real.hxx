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


#include <uhal/log/log.hpp>

#include <stdlib.h>

namespace uhal
{

  template< typename T , uint32_t WIDTH >
  void log_inserter ( const _Real< T , RealFmt<WIDTH> >& aReal )
  {
    char lBuffer[ WIDTH+10 ]; // Make the buffer plenty wide enough, since the exponent is not counted in the width term!
    // An IEEE double has an 11=bit exponent = -1023 to 1024, adding the "e" character means that the
    // maximum size of the exponent is 6 characters. 10 is a nice round number and is on the safe side.
    gcvt ( aReal.value() , WIDTH , lBuffer );
    put ( lBuffer );
  }

  template< typename FORMAT >
  struct RealFactory < double , FORMAT >
  {
    static _Real< double , FORMAT > Construct ( const double& aReal )
    {
      return _Real< double , FORMAT > ( aReal );
    }
  };

  template< typename FORMAT >
  struct RealFactory < float , FORMAT >
  {
    static _Real< float , FORMAT > Construct ( const float& aReal )
    {
      return _Real< float , FORMAT > ( aReal );
    }
  };


  template< typename T >
  _Real< T , RealFmt<> > Real ( const T& aT )
  {
    return RealFactory< T , RealFmt<> >::Construct ( aT );
  }


  template< typename T , typename FORMAT >
  _Real< T , FORMAT > Real ( const T& aT , const FORMAT& aFmt )
  {
    return RealFactory< T , FORMAT >::Construct ( aT );
  }

}

