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


#ifndef _log_inserters_real_hpp_
#define _log_inserters_real_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{

  static const uint32_t DefaultRealWidth ( 10 );

  template< typename T , typename FORMAT > struct RealFactory;

  template< uint32_t WIDTH = DefaultRealWidth > struct RealFmt {};

  template< typename T , typename FORMAT >
  class _Real : public RefWrapper< T >
  {
      friend class RealFactory< T , FORMAT >;
      _Real ( const T& aReal ) : RefWrapper< T > ( aReal ) {}
  };

  template< typename T > _Real< T , RealFmt<> > Real ( const T& aT );
  template< typename T , typename FORMAT > _Real< T , FORMAT > Real ( const T& aT , const FORMAT& aFmt );

}

#include <uhal/log/log_inserters.real.hxx>

#endif
