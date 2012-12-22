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


#ifndef _log_inserters_boolean_hpp_
#define _log_inserters_boolean_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <string>

namespace uhal
{

  enum boolean_format
  {
    alpha,
    numeric
  };

  static const boolean_format DefaultBooleanFormat ( alpha );

  template< typename T , typename FORMAT > struct BooleanFactory;

  template< boolean_format FORMAT = DefaultBooleanFormat > struct BoolFmt {};

  template< typename T , typename FORMAT >
  class _Boolean : public RefWrapper< T >
  {
      friend class BooleanFactory< T , FORMAT >;
      _Boolean ( const T& aT ) : RefWrapper< T > ( aT ) {}
  };


  template< typename T > _Boolean< T , BoolFmt<> > Boolean ( const T& aT );

  template< typename T , typename FORMAT > _Boolean< T , FORMAT > Boolean ( const T& aT , const FORMAT& aFmt );

}

#include <uhal/log/log_inserters.boolean.hxx>

#endif
