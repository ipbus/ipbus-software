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
#include <uhal/log/log_inserters.integer.hpp>
#include <uhal/log/log_inserters.type.hpp>


namespace uhal
{

  template< typename T >
  void log_inserter ( const _Pointer< T >& aPointer )
  {
    put ( '(' );
    log_inserter ( Type< T >() );
    put ( ")(0x" );
    static const char* lCharacterMapping ( "0123456789ABCDEF" );
    uint64_t lPointer ( ( uint64_t ) ( aPointer.value() ) );
    uint8_t* lStart ( ( uint8_t* ) ( &lPointer ) );
    uint8_t* lPtr ( lStart + sizeof ( T* ) );

    do
    {
      --lPtr;
      put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0xF0 ) >>4 ) ) );
      put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0x0F ) ) ) );
    }
    while ( lPtr!=lStart );

    put ( ")(+" );
    log_inserter ( Integer ( sizeof ( T ) ) );
    put ( ')' );
  }


  template< typename T >
  _Pointer< T > Pointer ( const T* aT )
  {
    return _Pointer< T > ( aT );
  }

}

