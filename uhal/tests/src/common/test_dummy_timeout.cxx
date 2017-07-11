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

#include "uhal/uhal.hpp"

#include "uhal/ProtocolUDP.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolControlHub.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;




void check_timeout ( const std::string& connection, const std::string& id, int sleepAfterFirstDispatch )
{
  ConnectionManager manager ( connection );
  HwInterface hw = manager.getDevice ( id );

  // Check we get an exception when first packet timeout occurs (dummy hardware only has delay on first packet)
  if ( hw.uri().find ( "ipbusudp" ) != std::string::npos )
  {
    CACTUS_TEST_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::UdpTimeout );
  }
  else if ( hw.uri().find ( "ipbustcp" ) != std::string::npos )
  {
    CACTUS_TEST_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::TcpTimeout );
  }
  else
  {
    CACTUS_TEST_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::ControlHubTargetTimeout );
  }

  std::cout << "Sleeping for " << sleepAfterFirstDispatch << " seconds to allow DummyHardware to clear itself" << std::endl;
  sleep ( sleepAfterFirstDispatch );
  // Check we can continue as normal without further exceptions.
  uint32_t x = static_cast<uint32_t> ( rand() );
  ValWord<uint32_t> y;
  CACTUS_TEST_NOTHROW (
    hw.getNode ( "REG" ).write ( x );
    y = hw.getNode ( "REG" ).read();
    hw.dispatch();
  );
  CACTUS_CHECK ( x==y );
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  CACTUS_TEST ( check_timeout ( connection_file, device_id, 3 ) );
  CACTUS_TEST_RESULT();
}
