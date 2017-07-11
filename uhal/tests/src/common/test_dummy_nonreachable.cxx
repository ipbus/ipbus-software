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




void check_nonreachable ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw = manager.getDevice ( id );

  // Check we get an exception corresponding to target being unreachable
  if ( hw.uri().find ( "ipbusudp" ) != std::string::npos )
  {
    CACTUS_TEST_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::UdpTimeout );
  }
  else if ( hw.uri().find ( "ipbustcp" ) != std::string::npos )
  {
    CACTUS_TEST_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::TcpConnectionFailure );
  }
  else
  {
    try
    {
      hw.getNode ( "REG" ).read();
      hw.dispatch();
      //Make a mock "TEST_THROW error message and record the failure to throw
      std::cerr << "TEST_THROW FAILED by NOT THROWING @" << __FILE__ << ":" << __LINE__ << std::endl;
      uhal::tests::failedTestCount++;
    }
    catch ( uhal::exception::exception& e )
    {
      // std::cout << "Exception of type " << typeid ( e ).name() << " was thrown" << std::endl;
      CACTUS_CHECK ( ( ( typeid ( e ) ==typeid ( uhal::exception::ControlHubTargetTimeout ) ) || ( typeid ( e ) ==typeid ( uhal::exception::TcpConnectionFailure ) ) ) );
    }
  }
}


int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  for ( size_t i = 0; i < 10; i++ )
  {
    CACTUS_TEST ( check_nonreachable ( connection_file, device_id ) );
  }
  CACTUS_TEST_RESULT();
}
