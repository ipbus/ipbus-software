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


BOOST_AUTO_TEST_CASE(check_nonreachable_device)
{
  for (size_t i = 0; i < 10; i++) {
    ConnectionManager manager ( TestFixture::sConnectionFile );
    HwInterface hw = manager.getDevice ( TestFixture::sDeviceId );

    // Check we get an exception corresponding to target being unreachable
    if ( hw.uri().find ( "ipbustcp" ) != std::string::npos )
    {
      BOOST_CHECK_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::TransportLayerError );
    }
    else
    {
      BOOST_CHECK_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::ClientTimeout );
    }
  }
}


HwInterface getHwWithModifiedControlHubPort(const std::string& aConnectionFile, const std::string& aDeviceId)
{
  typedef boost::filesystem::path Path_t;
  const std::string lAddrFilePath = ( Path_t(aConnectionFile).parent_path() / Path_t("dummy_address.xml") ).string();

  const std::string lOriginalUri = ConnectionManager(aConnectionFile).getDevice(aDeviceId).getClient().uri();
  const std::string lModifiedUri = lOriginalUri.substr(0, lOriginalUri.find("?")-1) + lOriginalUri.substr(lOriginalUri.find("?"));

  return ConnectionManager::getDevice(aDeviceId, lModifiedUri, lAddrFilePath);  
}

BOOST_AUTO_TEST_CASE(check_nonreachable_controlhub)
{
  if ( (TestFixture::sDeviceInfo.type == IPBUS_1_3_CONTROLHUB) || (TestFixture::sDeviceInfo.type == IPBUS_2_0_CONTROLHUB) ) {
    for (size_t i = 0; i < 10; i++) {
      HwInterface hw = getHwWithModifiedControlHubPort(TestFixture::sConnectionFile, TestFixture::sDeviceId);

      BOOST_CHECK_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::TransportLayerError );
    }
  }
  else
    BOOST_TEST_MESSAGE("  ***  Skipping check_nonreachable_controlhub test case, since client under test does not talk to ControlHub.  ***");
}


BOOST_AUTO_TEST_SUITE_END()

} // end ns tests
} // end ns uhal
