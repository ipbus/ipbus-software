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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/uhal.hpp"

#include "uhal/ProtocolUDP.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolControlHub.hpp"

#include "uhal/tests/tools.hpp"

#include <boost/test/unit_test.hpp>

#include <iostream>
#include <cstdlib>
#include <typeinfo>


namespace uhal {
namespace tests {

BOOST_AUTO_TEST_SUITE(NonreachableTestSuite)


// FIXME: Add duplicate nonreachable test covering ControlHub being nonreachable

BOOST_AUTO_TEST_CASE(check_nonreachable)
{
  for (size_t i = 0; i < 10; i++) {
    ConnectionManager manager ( TestFixture::sConnectionFile );
    HwInterface hw = manager.getDevice ( TestFixture::sDeviceId );

    // Check we get an exception corresponding to target being unreachable
    if ( hw.uri().find ( "ipbusudp" ) != std::string::npos )
    {
      BOOST_CHECK_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::UdpTimeout );
    }
    else if ( hw.uri().find ( "ipbustcp" ) != std::string::npos )
    {
      BOOST_CHECK_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::TcpConnectionFailure );
    }
    else
    {
      try
      {
        hw.getNode ( "REG" ).read();
        hw.dispatch();
        //Make a mock "TEST_THROW error message and record the failure to throw
        BOOST_CHECK(false);
        std::cerr << "TEST_THROW FAILED by NOT THROWING @" << __FILE__ << ":" << __LINE__ << std::endl;
        uhal::tests::failedTestCount++;
      }
      catch ( uhal::exception::exception& e )
      {
        // std::cout << "Exception of type " << typeid ( e ).name() << " was thrown" << std::endl;
        BOOST_CHECK ( ( ( typeid ( e ) ==typeid ( uhal::exception::ControlHubTargetTimeout ) ) || ( typeid ( e ) ==typeid ( uhal::exception::TcpConnectionFailure ) ) ) );
      }
    }
  }
}


BOOST_AUTO_TEST_SUITE_END()

} // end ns tests
} // end ns uhal
