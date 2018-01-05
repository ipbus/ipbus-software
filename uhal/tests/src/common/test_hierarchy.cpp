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

#include <vector>
#include <iostream>
#include <cstdlib>
#include <typeinfo>


#define N_1MB    uint32_t(1024*1024/4)


namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(NodeHierarchyTestSuite, write_read_hierarchy, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  //check non-overlapping addresses
  BOOST_REQUIRE ( hw.getNode ( "SUBSYSTEM1.REG" ).getAddress() != hw.getNode ( "SUBSYSTEM2.REG" ).getAddress() );
  BOOST_REQUIRE ( hw.getNode ( "SUBSYSTEM1.MEM" ).getAddress() != hw.getNode ( "SUBSYSTEM2.MEM" ).getAddress() );
  //create transactions
  uint32_t x1 = static_cast<uint32_t> ( rand() );
  hw.getNode ( "SUBSYSTEM1.REG" ).write ( x1 );
  ValWord< uint32_t > reg1 = hw.getNode ( "SUBSYSTEM1.REG" ).read();
  uint32_t x2 = static_cast<uint32_t> ( rand() );
  hw.getNode ( "SUBSYSTEM2.REG" ).write ( x2 );
  ValWord< uint32_t > reg2 = hw.getNode ( "SUBSYSTEM2.REG" ).read();
  std::vector<uint32_t> xx1;

  for ( size_t i=0; i!= N_1MB; ++i )
  {
    xx1.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  hw.getNode ( "SUBSYSTEM1.MEM" ).writeBlock ( xx1 );
  ValVector< uint32_t > mem1 = hw.getNode ( "SUBSYSTEM1.MEM" ).readBlock ( N_1MB );
  std::vector<uint32_t> xx2;

  for ( size_t i=0; i!= N_1MB; ++i )
  {
    xx2.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  hw.getNode ( "SUBSYSTEM2.MEM" ).writeBlock ( xx2 );
  ValVector< uint32_t > mem2 = hw.getNode ( "SUBSYSTEM2.MEM" ).readBlock ( N_1MB );
  BOOST_CHECK ( !mem2.valid() );
  BOOST_CHECK_EQUAL ( mem1.size(), N_1MB );
  BOOST_CHECK_EQUAL ( mem2.size(), N_1MB );
  // CACTUS_TEST_THROW ( mem1.at ( rand() % N_1MB ),uhal::exception::NonValidatedMemory );      // This precondition is false because of the pre-emptive dispatch
  BOOST_CHECK_THROW ( mem2.at ( rand() % N_1MB ),uhal::exception::NonValidatedMemory );
  //send packet
  BOOST_CHECK_NO_THROW ( hw.dispatch() );
  //check results
  BOOST_CHECK_EQUAL ( reg1.value(), x1 );
  bool correct_block_write_read_subsystem1 = true;
  ValVector< uint32_t >::const_iterator i1=mem1.begin();
  std::vector< uint32_t >::const_iterator j1=xx1.begin();

  for ( ; i1!=mem1.end(); ++i1 , ++j1 )
  {
    correct_block_write_read_subsystem1 = correct_block_write_read_subsystem1 && ( *i1 == *j1 );
  }

  BOOST_CHECK_EQUAL ( mem1.size(), N_1MB );
  BOOST_CHECK ( correct_block_write_read_subsystem1 );
  BOOST_CHECK_EQUAL ( reg2.value(), x2 );
  bool correct_block_write_read_subsystem2 = true;
  ValVector< uint32_t >::const_iterator i2=mem2.begin();
  std::vector< uint32_t >::const_iterator j2=xx2.begin();

  for ( ; i2!=mem2.end(); ++i2 , ++j2 )
  {
    correct_block_write_read_subsystem2 = correct_block_write_read_subsystem2 && ( *i2 == *j2 );
  }

  BOOST_CHECK_EQUAL ( mem2.size(), N_1MB );
  BOOST_CHECK ( correct_block_write_read_subsystem2 );
}
)


} // end ns tests
} // end ns uhal

