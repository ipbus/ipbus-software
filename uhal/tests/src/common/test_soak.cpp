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

#include "uhal/tests/PerfTester.hxx"
#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>

#include <vector>
#include <algorithm>
#include <string>
#include <iostream>
#include <cstdlib>
#include <typeinfo>


namespace uhal {
namespace tests {


void report_rx_performance(ClientInterface& aClient, const uint32_t aBaseAddr, const uint32_t aDepth, const size_t aNrIterations, const bool aDispatchEachIteration)
{
  double totalSeconds = measureReadLatency(aClient, aBaseAddr, aDepth, aNrIterations, aDispatchEachIteration, false);
  double totalPayloadKB = aNrIterations * aDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;
  std::cout << " --> " << aNrIterations << " reads, each " << aDepth << " x 32-bit words, took " << totalSeconds << " seconds" << std::endl
            << "Total IPbus payload received    = " << totalPayloadKB << " KB\n"
            << "Average read bandwidth          = " << dataRateKB_s << " KB/s" << std::endl;
}


void report_tx_performance(ClientInterface& aClient, const uint32_t aBaseAddr, const uint32_t aDepth, const size_t aNrIterations, const bool aDispatchEachIteration)
{
  double totalSeconds = measureWriteLatency(aClient, aBaseAddr, aDepth, aNrIterations, aDispatchEachIteration, false);
  double totalPayloadKB = aNrIterations * aDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;
  std::cout << " --> " << aNrIterations << " writes, each " << aDepth << " x 32-bit words, took " << totalSeconds << " seconds" << std::endl
            << "Total IPbus payload received    = " << totalPayloadKB << " KB\n"
            << "Average read bandwidth          = " << dataRateKB_s << " KB/s" << std::endl;

}


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(SoakTestSuite, bandwidth_rx, DummyHardwareFixture,
{
  if (quickTest)
    BOOST_TEST_MESSAGE("  ***  Skipping soak tests since '--quick' flag was used  ***");
  else {
    HwInterface hw = getHwInterface();

    BOOST_CHECK_NO_THROW( report_rx_performance(hw.getClient(), 0x01, 1, 100, true) );
    BOOST_CHECK_NO_THROW( report_rx_performance(hw.getClient(), 0x01, 262144, 100, true) );
  }
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(SoakTestSuite, bandwidth_tx, DummyHardwareFixture,
{
  if (quickTest)
    BOOST_TEST_MESSAGE("  ***  Skipping soak tests since '--quick' flag was used  ***");
  else {

    HwInterface hw = getHwInterface();

    BOOST_CHECK_NO_THROW( report_tx_performance(hw.getClient(), 0x01, 1, 100, true) );
    BOOST_CHECK_NO_THROW( report_tx_performance(hw.getClient(), 0x01, 262144, 100, true) );
  }
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(SoakTestSuite, quick_soak, DummyHardwareFixture,
{
  if (quickTest)
    BOOST_TEST_MESSAGE("  ***  Skipping soak tests since '--quick' flag was used  ***");
  else {
    HwInterface hw = getHwInterface();

    std::vector<ClientInterface*> lClients;
    lClients.push_back(&hw.getClient());

    BOOST_CHECK( PerfTester::runValidationTest(lClients, 0x1000, 1024, 2000, false, false) );
  }
}
)


} // end ns tests
} // end ns uhal

