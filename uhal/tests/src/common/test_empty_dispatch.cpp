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
#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/test/unit_test.hpp>


namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(EmptyDispatchTestSuite, empty_dispatch, MinimalFixture,
{
  HwInterface hw = getHwInterface();
  BOOST_CHECK_NO_THROW ( hw.dispatch() );
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(EmptyDispatchTestSuite, empty_dispatch_after_read, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  BOOST_CHECK_NO_THROW ( ValWord< uint32_t > r = hw.getNode ( "REG" ).read() );
  BOOST_CHECK_NO_THROW ( hw.dispatch() );
  BOOST_CHECK_NO_THROW ( hw.dispatch() );
}
)


} // end ns tests
} // end ns uhal
