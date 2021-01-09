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


#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>
#include <typeinfo>
#include <vector>

#include <boost/test/unit_test.hpp>

#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"


namespace uhal {
namespace tests {


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(RawClientTestSuite, single_write_read, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  ClientInterface* c = &hw.getClient();
  uint32_t x = static_cast<uint32_t> ( rand() );
  uint32_t addr = hw.getNode ( "REG" ).getAddress();
  c->write ( addr,x );
  ValWord< uint32_t > reg = c->read ( addr );
  BOOST_CHECK ( !reg.valid() );
  BOOST_CHECK_THROW ( reg.value(),uhal::exception::NonValidatedMemory );
  c->dispatch();
  BOOST_CHECK ( reg.valid() );
  BOOST_CHECK_EQUAL ( reg.value(), x );
  BOOST_CHECK_THROW ( c->write ( addr,0xF0000000, 0xF0 ) ,uhal::exception::BitsSetWhichAreForbiddenByBitMask );
  BOOST_CHECK_THROW ( c->write ( addr,0xFF, 0x0F ) ,uhal::exception::BitsSetWhichAreForbiddenByBitMask );
  uint32_t y = static_cast<uint32_t> ( rand() ) & 0xF;
  c->write ( addr,y, 0xF );
  ValWord< uint32_t > reg2 = c->read ( addr );
  BOOST_CHECK ( !reg2.valid() );
  BOOST_CHECK_THROW ( reg2.value(),uhal::exception::NonValidatedMemory );
  c->dispatch();
  BOOST_CHECK ( reg2.valid() );
  BOOST_CHECK_EQUAL ( reg2.value(), ( ( x&~0xF ) |y ) );
  ValWord< uint32_t > reg3 = c->read ( addr , 0xF );
  BOOST_CHECK ( !reg3.valid() );
  BOOST_CHECK_THROW ( reg3.value(),uhal::exception::NonValidatedMemory );
  c->dispatch();
  BOOST_CHECK ( reg3.valid() );
  BOOST_CHECK_EQUAL ( reg3.value(), y );
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(RawClientTestSuite, mem_write_read, DummyHardwareFixture,
{
  const uint32_t N =1024*1024/4;
  HwInterface hw = getHwInterface();
  ClientInterface* c = &hw.getClient();
  std::vector<uint32_t> xx;

  for ( size_t i=0; i!= N; ++i )
  {
    xx.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  uint32_t addr = hw.getNode ( "MEM" ).getAddress();
  c->writeBlock ( addr, xx );
  ValVector< uint32_t > mem = c->readBlock ( addr, N );
  BOOST_CHECK ( !mem.valid() );
  BOOST_CHECK_EQUAL ( mem.size(), N );
  BOOST_CHECK_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
  c->dispatch();
  BOOST_CHECK ( mem.valid() );
  BOOST_CHECK_EQUAL ( mem.size(), N );
  bool correct_block_write_read = true;

  std::vector< uint32_t >::const_iterator j=xx.begin();
  for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
  {
    correct_block_write_read = correct_block_write_read && ( *i == *j );
  }

  BOOST_CHECK ( correct_block_write_read );
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(RawClientTestSuite, mem_rmw_bits, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();
  ClientInterface* c = &hw.getClient();
  uint32_t addr = hw.getNode ( "REG_UPPER_MASK" ).getAddress();
  uint32_t x1 = static_cast<uint32_t> ( rand() );
  uint32_t x2 = static_cast<uint32_t> ( rand() );
  uint32_t x3 = static_cast<uint32_t> ( rand() );
  c->write ( addr,x1 );
  ValWord< uint32_t > reg1 = c->rmw_bits ( addr,x2,x3 );
  ValWord< uint32_t > reg2 = c->read ( addr );
  c->dispatch();
  BOOST_CHECK_EQUAL ( ( ( x1 & x2 ) | x3 ), reg2.value() );

  //IPBus 1.3 bug on RMW: https://svnweb.cern.ch/trac/cactus/ticket/179
  if ( hw.uri().find ( "ipbusudp-1.3://" ) != std::string::npos ||
       hw.uri().find ( "ipbustcp-1.3://" ) != std::string::npos ||
       hw.uri().find ( "chtcp-1.3://" ) != std::string::npos )
  {
    BOOST_CHECK_EQUAL ( reg1.value(), ( ( x1 & x2 ) | x3 ) );
  }
  else
  {
    BOOST_CHECK_EQUAL ( reg1.value(), x1 );
  }
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(RawClientTestSuite, mem_rmw_sum, DummyHardwareFixture,
{
  const uint32_t N =1024;
  HwInterface hw = getHwInterface();
  ClientInterface* c = &hw.getClient();
  uint32_t total = 0;
  std::vector<uint32_t> xx;
  bool IPbus1_3;

  //IPBus 1.3 bug on RMW: https://svnweb.cern.ch/trac/cactus/ticket/179
  if ( hw.uri().find ( "ipbusudp-1.3://" ) != std::string::npos ||
       hw.uri().find ( "ipbustcp-1.3://" ) != std::string::npos ||
       hw.uri().find ( "chtcp-1.3://" ) != std::string::npos )
  {
    IPbus1_3=true;
  }
  else
  {
    IPbus1_3=false;
  }

  uint32_t x ( 0x00000000 );

  for ( size_t i=0; i!= N; ++i )
  {
    if ( !IPbus1_3 )
    {
      total += x;
      x = static_cast<uint32_t> ( rand() );
    }
    else
    {
      x = static_cast<uint32_t> ( rand() );
      total += x;
    }

    xx.push_back ( x );
  }

  uint32_t addr = hw.getNode ( "SUBSYSTEM1.REG" ).getAddress();
  c->write ( addr,xx[0] );
  ValWord<uint32_t> reg;

  for ( size_t i=1; i!= N; ++i )
  {
    reg = c->rmw_sum ( addr,xx[i] );
    c->dispatch();
  }

  BOOST_CHECK_EQUAL ( reg.value(), total );
}
)


} // end ns tests
} // end ns uhal

