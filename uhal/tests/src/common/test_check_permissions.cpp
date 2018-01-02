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

#include <iostream>
#include <cstdlib>
#include <typeinfo>


namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(CheckPermissionsTestSuite, check_permissions, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  BOOST_CHECK_THROW ( hw.getNode ( "REG_READ_ONLY" ).write ( 1 ),uhal::exception::WriteAccessDenied );
  BOOST_CHECK_THROW ( hw.getNode ( "REG_MASKED_READ_ONLY" ).write ( 1 ),uhal::exception::WriteAccessDenied );
  BOOST_CHECK_THROW ( hw.getNode ( "REG_WRITE_ONLY" ).read(),uhal::exception::ReadAccessDenied );
  BOOST_CHECK_THROW ( hw.getNode ( "REG_MASKED_WRITE_ONLY" ).read(),uhal::exception::ReadAccessDenied );
  BOOST_CHECK_THROW ( hw.getNode ( "REG_MASKED_WRITE_ONLY" ).write ( 1 ),uhal::exception::WriteAccessDenied );

  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG_WRITE_ONLY" ).write ( x );
  ValWord< uint32_t > mem = hw.getNode ( "REG_READ_ONLY" ).read();
  ValWord< uint32_t > mem2 = hw.getNode ( "REG_MASKED_READ_ONLY" ).read();
  hw.dispatch();
}
)


} // end ns tests
} // end ns uhal
