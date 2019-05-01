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

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

---------------------------------------------------------------------------
*/


#include "uhal/tests/DummyHardwareOptions.hpp"
#include "uhal/tests/TCPDummyHardware.hpp"


using namespace uhal;
using namespace uhal::tests;

int main ( int argc, char* argv[] )
{
  DummyHardwareOptions lOptions ( DummyHardwareOptions::parseFromCommandLine ( argc , argv ) );

  if ( lOptions.version == 1 )
  {
    TCPDummyHardware<1,3> lDummyHardware ( lOptions.port , lOptions.delay , false );

    while ( true ) //Is this necessary with the nested "while(true)"'s in the run function?
    {
      lDummyHardware.run();
    }
  }
  else if ( lOptions.version == 2 )
  {
    TCPDummyHardware<2,0> lDummyHardware ( lOptions.port , lOptions.delay, lOptions.bigendian );

    while ( true ) //Is this necessary with the nested "while(true)"'s in the run function?
    {
      lDummyHardware.run();
    }
  }
  else
  {
    log ( Error() , "Unknown IPbus version, " , Integer ( lOptions.version ) );
    return 1;
  }

  return 0;
}
