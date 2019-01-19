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
#include "uhal/ProtocolPCIe.hpp"
#include "uhal/ProtocolControlHub.hpp"

#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/chrono/chrono_io.hpp>
#include <boost/test/unit_test.hpp>

#include <iostream>
#include <cstdlib>
#include <typeinfo>


namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(TimeoutTestSuite, check_timeout, DummyHardwareFixture,
{
  hwRunner.setReplyDelay( boost::chrono::milliseconds(timeout) + boost::chrono::seconds(1) );
  HwInterface hw = getHwInterface();

  // Check we get an exception when first packet timeout occurs (dummy hardware only has delay on first packet)
  BOOST_CHECK_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception::ClientTimeout );

  const boost::chrono::milliseconds sleepDuration = boost::chrono::milliseconds(timeout) + boost::chrono::seconds(1);
  BOOST_TEST_MESSAGE("Sleeping for " << sleepDuration << " seconds to allow DummyHardware to clear itself");
  boost::this_thread::sleep_for(sleepDuration);
  // Check we can continue as normal without further exceptions.
  uint32_t x = static_cast<uint32_t> ( rand() );
  ValWord<uint32_t> y;
  BOOST_CHECK_NO_THROW (
    hw.getNode ( "REG" ).write ( x );
    y = hw.getNode ( "REG" ).read();
    hw.dispatch();
  );
  BOOST_CHECK ( x == y );
}
)


} // end ns tests
} // end ns uhal
