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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/utilities/bits.hpp"



namespace uhal
{
  namespace utilities
  {
    unsigned int TrailingRightBits ( uint32_t aValue )
    {
      // unsigned int lReturn = sizeof ( aValue ) * 8; // lReturn will be the number of zero bits on the right
      unsigned int lReturn = sizeof ( aValue ) << 3; // lReturn will be the number of zero bits on the right
      aValue &= -signed ( aValue );

      if ( aValue )
      {
        lReturn--;
      }

      if ( aValue & 0x0000FFFF )
      {
        lReturn -= 16;
      }

      if ( aValue & 0x00FF00FF )
      {
        lReturn -= 8;
      }

      if ( aValue & 0x0F0F0F0F )
      {
        lReturn -= 4;
      }

      if ( aValue & 0x33333333 )
      {
        lReturn -= 2;
      }

      if ( aValue & 0x55555555 )
      {
        lReturn -= 1;
      }

      return lReturn;
    }
  }
}
