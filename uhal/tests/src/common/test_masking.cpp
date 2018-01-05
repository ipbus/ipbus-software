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
#include <ios>
#include <cstdlib>
#include <typeinfo>


namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(MaskedNodeTestSuite, write_read_masked, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG_LOWER_MASK" ).write ( x & 0xFFFF );
  hw.getNode ( "REG_UPPER_MASK" ).write ( x >> 16 );
  BOOST_CHECK_THROW ( hw.getNode ( "REG_LOWER_MASK" ).write ( 0x1FFFF ),uhal::exception::exception );
  BOOST_CHECK_THROW ( hw.getNode ( "REG_UPPER_MASK" ).write ( 0x1FFFF ),uhal::exception::exception );
  BOOST_CHECK_EQUAL ( hw.getNode ( "REG_LOWER_MASK" ).getMask(), 0xFFFFu );
  BOOST_CHECK_EQUAL ( hw.getNode ( "REG_UPPER_MASK" ).getMask(), 0xFFFF0000u );
  ValWord<uint32_t> reg_l = hw.getNode ( "REG_LOWER_MASK" ).read();
  ValWord<uint32_t> reg_u = hw.getNode ( "REG_UPPER_MASK" ).read();
  BOOST_CHECK ( !reg_l.valid() && !reg_u.valid() );
  BOOST_CHECK_THROW ( reg_l.value(),uhal::exception::exception );
  BOOST_CHECK_THROW ( reg_u.value(),uhal::exception::exception );
  BOOST_CHECK_NO_THROW ( hw.dispatch() );
  BOOST_CHECK ( reg_l.value() <= 0xFFFF );
  BOOST_CHECK ( reg_u.value() <= 0xFFFF );
  BOOST_CHECK ( reg_l.valid() && reg_u.valid() );
  BOOST_CHECK_EQUAL ( reg_l.value(), ( x & 0xFFFF ) );
  BOOST_CHECK_EQUAL ( reg_u.value(), ( x >> 16 ) );
}
)


} // end ns tests
} // end ns uhal
