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
#include "uhal/ProtocolIPbusCore.hpp"

#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/test/unit_test.hpp>



namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(ConfigSpaceTestSuite, read_fullWord, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  IPbusCore& client = dynamic_cast<IPbusCore&>(hw.getClient());

  for (size_t i = 0; i < 10; i++) {
    const uint32_t expectedValue = uint16_t(getpid()) << 16 | i;

    switch (deviceType) {
      case IPBUS_1_3_UDP :
      case IPBUS_1_3_TCP :
      case IPBUS_1_3_CONTROLHUB :
        BOOST_CHECK_THROW(client.readConfigurationSpace(i), exception::ValidationError);
        break;
      default:
        ValWord<uint32_t> x = client.readConfigurationSpace(i);

        client.dispatch();
        BOOST_CHECK_EQUAL(x.value(), expectedValue);
    }
  }
}
)

UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(ConfigSpaceTestSuite, read_masked, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  IPbusCore& client = dynamic_cast<IPbusCore&>(hw.getClient());

  for (size_t i = 0; i < 10; i++) {
    const uint32_t expectedValue = uint16_t(getpid()) << 16 | i;

    switch (deviceType) {
      case IPBUS_1_3_UDP :
      case IPBUS_1_3_TCP :
      case IPBUS_1_3_CONTROLHUB :
        BOOST_CHECK_THROW(ValWord<uint32_t> x_lower = client.readConfigurationSpace(i, 0xFF), exception::ValidationError);
        break;
      default:
        ValWord<uint32_t> x_lower = client.readConfigurationSpace(i, 0xFF);
        ValWord<uint32_t> x_mid   = client.readConfigurationSpace(i, 0xFFFF00);
        ValWord<uint32_t> x_upper = client.readConfigurationSpace(i, 0xFF000000);

        client.dispatch();
        BOOST_CHECK_EQUAL(x_lower.value(), uint32_t(expectedValue & 0xFF));
        BOOST_CHECK_EQUAL(x_mid.value(), uint32_t((expectedValue & 0xFFFF00) >> 8));
        BOOST_CHECK_EQUAL(x_upper.value(), uint32_t((expectedValue & 0xFF000000) >> 24));
    }
  }
}
)

} // end ns tests
} // end ns uhal

