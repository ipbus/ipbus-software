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


using namespace uhal;

#define N_4B     1
#define N_1kB    1024/4
#define N_1MB    1024*1024/4
#define N_10MB   10*1024*1024/4
#define N_200MB  100*1024*1024/4


namespace uhal {
namespace tests {

std::vector<size_t> getBlockUnitTestDepths(const size_t aMaxSize)
{
  std::vector<size_t> lDepths;
  lDepths.push_back(0);
  lDepths.push_back(N_4B);
  lDepths.push_back(N_1kB);
  lDepths.push_back(10 * N_1kB);
  lDepths.push_back(100 * N_1kB);
  lDepths.push_back(N_1MB);
  lDepths.push_back(N_10MB);
  lDepths.push_back(N_200MB);

  for (std::vector<size_t>::iterator lIt=lDepths.begin(); lIt != lDepths.end(); ) {
    if ((*lIt) > aMaxSize)
      lIt = lDepths.erase(lIt);
    else
      ++lIt;
  }
  return lDepths;
}


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(BlockReadWriteTestSuite, block_write_read, DummyHardwareFixture,
{
  std::vector<size_t> lDepths = getBlockUnitTestDepths(quickTest ? N_1MB : N_10MB);

  for(size_t i=0; i<lDepths.size(); i++) {
    const size_t N = lDepths.at(i);
    BOOST_TEST_MESSAGE("  N = " << N);

    HwInterface hw = getHwInterface();

    std::vector<uint32_t> xx;
    xx.reserve ( N );
    for ( size_t i=0; i!= N; ++i )
    {
      xx.push_back ( static_cast<uint32_t> ( rand() ) );
    }

    hw.getNode ( "LARGE_MEM" ).writeBlock ( xx );
    ValVector< uint32_t > mem = hw.getNode ( "LARGE_MEM" ).readBlock ( N );
    BOOST_CHECK ( !mem.valid() );
    BOOST_CHECK_EQUAL ( mem.size(), N );
    if ( N > 0 )
    {
      BOOST_CHECK_THROW ( mem.at ( 0 ), uhal::exception::NonValidatedMemory );
    }
    BOOST_CHECK_THROW ( mem.value(), uhal::exception::NonValidatedMemory );
    BOOST_CHECK_NO_THROW ( hw.dispatch() );
    BOOST_CHECK ( mem.valid() );
    BOOST_CHECK_EQUAL ( mem.size(), N );

    //This check will fail when DummyHardware::ADDRESS_MASK < N
    if ( N < N_10MB )
    {
      bool correct_block_write_read = true;
      std::vector< uint32_t >::const_iterator j=xx.begin();

      for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
      {
        correct_block_write_read = correct_block_write_read && ( *i == *j );
      }

      BOOST_CHECK ( correct_block_write_read );
    }
  }
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(BlockReadWriteTestSuite, fifo_write_read, DummyHardwareFixture,
{
  std::vector<size_t> lDepths = getBlockUnitTestDepths(quickTest ? N_1MB : N_200MB);

  for(size_t i=0; i<lDepths.size(); i++) {
    const size_t N = lDepths.at(i);
    BOOST_TEST_MESSAGE("  N = " << N);

    HwInterface hw = getHwInterface();
    // Scope the large source vector so that the memory is freed up after the call to write. The data is safe, since it is copied into the send buffers.
    std::vector<uint32_t> xx;
    xx.reserve ( N );

    for ( size_t i=0; i!= N; ++i )
    {
      xx.push_back ( static_cast<uint32_t> ( rand() ) );
    }

    hw.getNode ( "FIFO" ).writeBlock ( xx );
    ValVector< uint32_t > mem = hw.getNode ( "FIFO" ).readBlock ( N );
    BOOST_CHECK ( !mem.valid() );
    BOOST_CHECK_EQUAL ( mem.size(), N );
    if ( N > 0 )
    {
      BOOST_CHECK_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
    }
    BOOST_CHECK_THROW ( mem.value(), uhal::exception::NonValidatedMemory );
    BOOST_CHECK_NO_THROW ( hw.dispatch() );
    BOOST_CHECK ( mem.valid() );
    BOOST_CHECK_EQUAL ( mem.size(), N );
    //The FIFO implementation on the dummy HW is a single memory location so there is not much to check
  }
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(BlockReadWriteTestSuite, block_offset_write_read, DummyHardwareFixture,
{
  std::vector<size_t> lDepths = getBlockUnitTestDepths(quickTest ? N_1MB : N_10MB);

  for(size_t i=0; i<lDepths.size(); i++) {
    const size_t N = lDepths.at(i);
    BOOST_TEST_MESSAGE("  N = " << N);

    HwInterface hw = getHwInterface();

    std::vector<uint32_t> xx;
    xx.reserve ( N );
    std::vector<uint32_t> yy;
    yy.reserve ( N );
    for ( size_t i=0; i!= N; ++i )
    {
      xx.push_back ( static_cast<uint32_t> ( rand() ) );
      yy.push_back ( static_cast<uint32_t> ( rand() ) );
    }

    hw.getNode ( "LARGE_MEM" ).writeBlockOffset ( xx , 0 );
    ValVector< uint32_t > mem = hw.getNode ( "LARGE_MEM" ).readBlockOffset ( N , 0 );

    BOOST_CHECK ( !mem.valid() );
    BOOST_CHECK_EQUAL ( mem.size(), N );

    if ( N > 0 )
    {
      BOOST_CHECK_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
    }
    BOOST_CHECK_THROW ( mem.value(), uhal::exception::NonValidatedMemory );


    hw.getNode ( "LARGE_MEM" ).writeBlockOffset ( yy , N );
    ValVector< uint32_t > mem2 = hw.getNode ( "LARGE_MEM" ).readBlockOffset ( N , N );

    BOOST_CHECK ( !mem2.valid() );
    BOOST_CHECK_EQUAL ( mem2.size(), N );

    if ( N > 0 )
    {
      BOOST_CHECK_THROW ( mem2.at ( 0 ),uhal::exception::NonValidatedMemory );
    }
    BOOST_CHECK_THROW ( mem2.value(), uhal::exception::NonValidatedMemory );

    BOOST_CHECK_NO_THROW ( hw.dispatch() );

    BOOST_CHECK ( mem.valid() );
    BOOST_CHECK_EQUAL ( mem.size(), N );

    BOOST_CHECK ( mem2.valid() );
    BOOST_CHECK_EQUAL ( mem2.size(), N );


    //This check will fail when DummyHardware::ADDRESS_MASK < N
    if ( N < N_10MB )
    {
      bool correct_block_write_read = true;
      std::vector< uint32_t >::const_iterator j=xx.begin();

      for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
      {
        correct_block_write_read = correct_block_write_read && ( *i == *j );
      }

      j=yy.begin();

      for ( ValVector< uint32_t >::const_iterator i ( mem2.begin() ); i!=mem2.end(); ++i , ++j )
      {
        correct_block_write_read = correct_block_write_read && ( *i == *j );
      }

      BOOST_CHECK ( correct_block_write_read );
    }
  }
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(BlockReadWriteTestSuite, block_access_type_violations, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();
  std::vector<uint32_t> xx;

  //We allow the user to call a bulk access of size=1 to a single register
  xx.resize ( N_4B );
  BOOST_CHECK_NO_THROW ( hw.getNode ( "REG" ).writeBlock ( xx ) );
  BOOST_CHECK_NO_THROW ( ValVector< uint32_t > mem = hw.getNode ( "REG" ).readBlock( N_4B ) );

  //These should all throw
  BOOST_CHECK_THROW ( hw.getNode ( "REG" ).writeBlockOffset ( xx , 0 ) , uhal::exception::BulkTransferOffsetRequestedForSingleRegister );
  BOOST_CHECK_THROW ( ValVector< uint32_t > mem = hw.getNode ( "REG" ).readBlockOffset ( N_1kB , 0 ) , uhal::exception::BulkTransferOffsetRequestedForSingleRegister );

  xx.resize ( N_1kB );
  BOOST_CHECK_THROW ( hw.getNode ( "REG" ).writeBlock ( xx ) , uhal::exception::BulkTransferOnSingleRegister );
  BOOST_CHECK_THROW ( ValVector< uint32_t > mem = hw.getNode ( "REG" ).readBlock( N_1kB ) , uhal::exception::BulkTransferOnSingleRegister );

  BOOST_CHECK_THROW ( hw.getNode ( "FIFO" ).writeBlockOffset ( xx , 1 ) , uhal::exception::BulkTransferOffsetRequestedForFifo );
  BOOST_CHECK_THROW ( ValVector< uint32_t > mem = hw.getNode ( "FIFO" ).readBlockOffset ( N_1kB , 1 ) , uhal::exception::BulkTransferOffsetRequestedForFifo );

}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(BlockReadWriteTestSuite, block_bigger_than_size_attribute, MinimalFixture,
{
  HwInterface hw = getHwInterface();
  std::vector<uint32_t> xx;
  xx.resize ( N_1MB );
  BOOST_CHECK_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlock ( xx ) , uhal::exception::BulkTransferRequestedTooLarge );
  BOOST_CHECK_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlock ( N_1MB ) , uhal::exception::BulkTransferRequestedTooLarge );
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(BlockReadWriteTestSuite, block_offset_bigger_than_size_attribute, MinimalFixture,
{
  HwInterface hw = getHwInterface();
  std::vector<uint32_t> xx;
  // Size OK but offset too large
  xx.resize ( N_4B );
  BOOST_CHECK_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlockOffset ( xx , 256 ) , uhal::exception::BulkTransferRequestedTooLarge );
  BOOST_CHECK_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlockOffset ( N_4B , 256 ) , uhal::exception::BulkTransferRequestedTooLarge );
  // Size OK, offset OK, combination too large
  xx.resize ( N_1kB );
  BOOST_CHECK_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlockOffset ( xx , 1 ) , uhal::exception::BulkTransferRequestedTooLarge );
  BOOST_CHECK_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlockOffset ( N_1kB , 1 ) , uhal::exception::BulkTransferRequestedTooLarge );
}
)


} // end ns tests
} // end ns uhal

