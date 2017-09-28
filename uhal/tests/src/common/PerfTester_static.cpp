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

#include "uhal/tests/PerfTester.hxx"

// C++ headers
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdlib>
#include <unistd.h>

// Boost headers
#include <boost/program_options.hpp>
#include <boost/bind.hpp>
#include <boost/foreach.hpp>
#include <boost/mem_fn.hpp>

// uHAL headers
#include "uhal/ClientFactory.hpp"
#include "uhal/tests/tools.hpp"

// Namespace resolution
using namespace std;




uint32_t uhal::tests::PerfTester::getRandomBlockSize ( const uint32_t maxSize )
{
  // Generate uniformly-distributed random float in range: 0 <= x < 1
  const double uniformRandom = static_cast<double> ( rand() ) / RAND_MAX; //TODO -- replace with boost::random random-double-generating function
  // Transform to 1/x distributed random float in range: 0.5 <= x < maxSize+1
  const double inverseRandom = 0.5 * pow ( static_cast<double> ( ( maxSize+1 ) / 0.5 ) , uniformRandom );
  uint32_t retVal = static_cast<uint32_t>( floor(inverseRandom) );

  if ( retVal > maxSize )
  {
    log ( Warning(), "Random block size (", Integer(retVal), ") is larger than maxSize (" , Integer(retVal) , ") ...\n",
                     "   * uniformRandom=", uniformRandom, "\n",
                     "   * inverseRandom=", inverseRandom, "\n",
                     "Correcting block size as temporary fix." );
    retVal = maxSize;
  }

  // Floor the float to get integer with desired distribution
  return retVal;
}


uhal::tests::PerfTester::U32Vec uhal::tests::PerfTester::getRandomBuffer ( unsigned size )
{
  U32Vec buffer;
  buffer.reserve ( size );

  // Never said anything about it being a flat random distribution... ;-)
  for ( unsigned i = 0 ; i < size ; ++i )
  {
    buffer.push_back ( rand() + rand() );
  }

  return buffer;
}


// Validation test -- single-register write/read-back
bool uhal::tests::PerfTester::validation_test_single_write_read ( ClientInterface& c, const uint32_t addr, const bool perTransactionDispatch, const bool aVerbose )
{
  std::ostringstream oss_details;
  oss_details << "Single-register write-read @ 0x" << std::hex << addr << ( perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if ( aVerbose )
  {
    cout << oss_details.str() << endl;
  }

  const uint32_t x = rand();

  try
  {
    c.write ( addr, x );

    if ( perTransactionDispatch )
    {
      c.dispatch();
    }

    ValWord<uint32_t> reg = c.read ( addr );
    c.dispatch();

    if ( x != reg.value() )
    {
      cout << "TEST FAILED: " << oss_details.str() << ". Wrote value 0x" << std::hex << x << " but read-back 0x" << reg.value() << std::dec << endl;
      return false;
    }
  }
  catch ( const std::exception& e )
  {
    cout << "TEST FAILED: " << oss_details.str() << ". Exception of type '" << typeid ( e ).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }

  return true;
}


// Validation test -- block write/read-back
bool uhal::tests::PerfTester::validation_test_block_write_read ( ClientInterface& c, const uint32_t addr, const uint32_t depth, const bool perTransactionDispatch, const bool aVerbose )
{
  std::ostringstream oss_details;
  oss_details << depth << "-word write-read @ 0x" << std::hex << addr;

  if ( depth>1 )
  {
    oss_details << " to 0x" << addr+depth-1 << std::dec;
  }

  oss_details << ( perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if ( aVerbose )
  {
    cout << "Running test: " << oss_details.str() << endl;
  }

  const U32Vec xx = getRandomBuffer ( depth );

  try
  {
    c.writeBlock ( addr, xx );

    if ( perTransactionDispatch )
    {
      c.dispatch();
    }

    U32ValVec ram = c.readBlock ( addr, depth );
    c.dispatch();
    std::vector<uint32_t>::const_iterator valVecIt = ram.begin();
    std::vector<uint32_t>::const_iterator xxIt = xx.begin();

    for ( ; valVecIt != ram.end(); valVecIt++, xxIt++ )
    {
      if ( ( *valVecIt ) != ( *xxIt ) )
      {
        uint32_t reg_addr = addr + ( valVecIt - ram.begin() );
        cout << "TEST FAILED: " << oss_details.str() << ". Wrote value Ox" << std::hex << *xxIt << " to register 0x" << reg_addr << " but read-back 0x" << *valVecIt << std::dec << std::endl;
        return false;
      }
    }

    log ( Notice(), "TEST PASSED: ", oss_details.str() );
  }
  catch ( const std::exception& e )
  {
    cout << "TEST FAILED: " << oss_details.str() << "! EXCEPTION of type '" << typeid ( e ).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }

  return true;
}


// Validation test -- write, RMW bits, read
bool uhal::tests::PerfTester::validation_test_write_rmwbits_read ( ClientInterface& c, const uint32_t addr, const bool perTransactionDispatch, const bool aVerbose )
{
  std::ostringstream oss_details;
  oss_details << "RMW-bits @ 0x" << std::hex << addr << ( perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if ( aVerbose )
  {
    cout << "TESTING: " << oss_details.str() << endl;
  }

  const uint32_t x0 = rand();

  const uint32_t a = rand();

  const uint32_t b = rand();

  const uint32_t x1 = ( x0 & a ) | b;

  std::ostringstream oss_values;

  oss_values << "Wrote value 0x" << std::hex << x0 << ", then did RMW-bits with AND-term 0x" << a << ", OR-term 0x" << b;

  const bool ipbus2 = ( ( c.uri().find ( "ipbusudp-2.0" ) != string::npos ) || ( c.uri().find ( "ipbustcp-2.0" ) != string::npos ) || ( c.uri().find ( "chtcp-2.0" ) != string::npos ) || ( c.uri().find ( "ipbuspcie-2.0" ) != string::npos ) );

  try
  {
    c.write ( addr, x0 );

    if ( perTransactionDispatch )
    {
      c.dispatch();
    }

    ValWord<uint32_t> reg_rmw = c.rmw_bits ( addr, a, b );
    c.dispatch();

    if ( ipbus2 ? ( x0 != reg_rmw.value() ) : ( x1 != reg_rmw.value() ) )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... RMW-bits returned value 0x" << std::hex << reg_rmw.value() << ", but expected 0x" << ( ipbus2 ? x0 : x1 ) << std::dec << endl << oss_values.str() << endl;
      return false;
    }

    ValWord<uint32_t> reg_read = c.read ( addr );
    c.dispatch();

    if ( x1 != reg_read.value() )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... Read after RMW-bits returned value 0x" << std::hex << reg_read.value() << ", but expected 0x" << x1 << std::dec << endl << oss_values.str() << endl;
      return false;
    }
  }
  catch ( const std::exception& e )
  {
    cout << "TEST FAILED: " << oss_values.str() << ". Exception of type '" << typeid ( e ).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }

  return true;
}

// Validation test -- write, RMW sum, read
bool uhal::tests::PerfTester::validation_test_write_rmwsum_read ( ClientInterface& c, const uint32_t addr, const bool perTransactionDispatch, const bool aVerbose )
{
  std::ostringstream oss_details;
  oss_details << "RMW-sum @ 0x" << std::hex << addr << ( perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if ( aVerbose )
  {
    cout << "TESTING: " << oss_details.str() << endl;
  }

  const uint32_t x0 = rand();

  const uint32_t a = rand();

  const uint32_t x1 = x0 + a;

  std::ostringstream oss_values;

  oss_values << "Wrote value 0x" << std::hex << x0 << ", then did RMW-sum with ADDEND 0x" << a;

  const bool ipbus2 = ( ( c.uri().find ( "ipbusudp-2.0" ) != string::npos ) || ( c.uri().find ( "ipbustcp-2.0" ) != string::npos ) || ( c.uri().find ( "chtcp-2.0" ) != string::npos )  || ( c.uri().find ( "ipbuspcie-2.0" ) != string::npos ) || ( c.uri().find ( "ipbuspcie-2.0" ) != string::npos ) );

  try
  {
    c.write ( addr, x0 );

    if ( perTransactionDispatch )
    {
      c.dispatch();
    }

    ValWord<uint32_t> reg_sum = c.rmw_sum ( addr, a );
    c.dispatch();

    if ( ipbus2 ? ( x0 != reg_sum.value() ) : ( x1 != reg_sum.value() ) )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... RMW-sum returned value 0x" << std::hex << reg_sum.value() << ", but expected 0x" << ( ipbus2 ? x0 : x1 ) << std::dec << endl << oss_values.str() << endl;
      return false;
    }

    ValWord<uint32_t> reg_read = c.read ( addr );
    c.dispatch();

    if ( x1 != reg_read.value() )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... Read after RMW-sum returned value 0x" << std::hex << reg_read.value() << ", but expected 0x" << x1 << std::dec << endl << oss_values.str() << endl;
      return false;
    }
  }
  catch ( const std::exception& e )
  {
    cout << "TEST FAILED: " << oss_values.str() << ". Exception of type '" << typeid ( e ).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }

  return true;
}


bool uhal::tests::PerfTester::runValidationTest(const std::vector<ClientInterface*>& aClients, const uint32_t aBaseAddr, const uint32_t aDepth, const size_t aNrIterations, const bool aDispatchEachIteration, const bool aVerbose)
{
  unsigned nrTestsFailed = 0;
  unsigned nrTestsTotal  = 0;

  // ---> A) Write read-back tests
  //
  typedef std::vector<ClientInterface*>::const_iterator ClientIt_t;
  for ( ClientIt_t clientIt = aClients.begin(); clientIt != aClients.end(); clientIt++ )
  {
    ClientInterface* client = *clientIt;
    cout << "\n\nWRITE READ-BACK TESTS for device at '" << client->uri() << "'" << endl;
    vector<uint32_t> addr_vec;
    addr_vec.push_back ( aBaseAddr );
    addr_vec.push_back ( aBaseAddr + aDepth - 1 );

    for ( unsigned i = 0; i < 2498; i++ )
    {
      addr_vec.push_back ( aBaseAddr + ( rand() % aDepth ) );
    }

    cout << "\n 1. Single-register write/read (" << addr_vec.size() * 2 << " tests)" << endl;

    for ( unsigned i = 0; i < addr_vec.size(); i++ )
    {
      nrTestsTotal++;

      if ( ! validation_test_single_write_read ( *client, addr_vec.at ( i ), true, aVerbose ) )
      {
        nrTestsFailed++;
      }

      nrTestsTotal++;

      if ( ! validation_test_single_write_read ( *client, addr_vec.at ( i ), false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
      }
    }

    cout << "\n 2. Block write/read (" << addr_vec.size() * 2 << " tests)" << endl;

    for ( unsigned i = 0; i < addr_vec.size(); i++ )
    {
      uint32_t addr = addr_vec.at ( i );
      uint32_t max_depth = aBaseAddr + aDepth - addr;
      uint32_t depth = rand() % ( max_depth + 1 );

      nrTestsTotal++;

      if ( ! validation_test_block_write_read ( *client, addr, depth, true, aVerbose ) )
      {
        nrTestsFailed++;
      }

      nrTestsTotal++;

      if ( ! validation_test_block_write_read ( *client, addr, depth, false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
      }
    }

    cout << "\n 3. Testing RMW-bits (write, RMWbits, read; " << addr_vec.size() * 2 << " tests)" << endl;

    for ( std::vector<uint32_t>::const_iterator it = addr_vec.begin(); it != addr_vec.end(); it++ )
    {
      nrTestsTotal++;

      if ( ! validation_test_write_rmwbits_read ( *client, *it, true, aVerbose ) )
      {
        nrTestsFailed++;
      }

      nrTestsTotal++;

      if ( ! validation_test_write_rmwbits_read ( *client, *it, false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
      }
    }

    cout << "\n 4. Testing RMW-sum (write, RMW-sum, read; " << addr_vec.size() * 2 << " tests)" << endl;

    for ( std::vector<uint32_t>::const_iterator it = addr_vec.begin(); it != addr_vec.end(); it++ )
    {
      nrTestsTotal++;

      if ( ! validation_test_write_rmwsum_read ( *client, *it, true, aVerbose ) )
      {
        nrTestsFailed++;
      }

      nrTestsTotal++;

      if ( ! validation_test_write_rmwsum_read ( *client, *it, false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
      }
    }
  }//end: for, m_clients

  if ( nrTestsFailed == 0 )
  {
    cout << "\n\nBASIC TESTS SUMMARY: All " << nrTestsTotal << " tests passed!" << endl << endl << endl;
  }
  else
  {
    cout << "\n\nBASIC TESTS SUMMARY: Total of " << nrTestsTotal << " tests run -- " << nrTestsFailed << " tests FAILED , " << ( nrTestsTotal - nrTestsFailed ) << " tests PASSED" << endl;
    return false;
  }

  // ---> B) SOAK TESTs
  //
  for ( ClientIt_t clientIt = aClients.begin(); clientIt != aClients.end(); clientIt++ )
  {
    ClientInterface* client = *clientIt;
    cout << "\nSOAK TEST to device '" << client->uri() << "'\n   Random sequence of " << aNrIterations << " transactions will be sent to hardware" << endl << endl;
    // Setup
    uint32_t ipbus_vsn;
    size_t found = client->uri().find ( "-1.3" );

    if ( found!=std::string::npos )
    {
      ipbus_vsn = 1;
    }
    else
    {
      found = client->uri().find ( "-2.0" );

      if ( found!=std::string::npos )
      {
        ipbus_vsn = 2;
      }
      else
      {
        log ( Error() , "Cannot deduce protocol from URI " , Quote ( client->uri() ), "  Exiting before performing soak test." );
        return false;
      }
    }

    // Initialise registers to 0x0
    std::vector< uint32_t > registers ( aDepth , 0x00000000 );
    client->writeBlock ( aBaseAddr, registers );
    client->dispatch();
    // ACTUAL MEAT OF SOAK TEST
    uint32_t type, addr, blockSize;
    uint32_t tempUInt1, tempUInt2;
    vector< boost::shared_ptr<QueuedTransaction> > queuedTransactions;
    uint32_t nrQueuedWords = 0;

    for ( unsigned i = 1; i <= aNrIterations; i++ )
    {
      type = ( rand() % 4 );
      addr = aBaseAddr + ( rand() % aDepth );

      switch ( type )
      {
        case 0: // read
        {
          blockSize = getRandomBlockSize ( aBaseAddr + aDepth - addr );
          log ( Notice(), "Soak test - queueing: ", Integer ( blockSize ), "-word read at ", Integer ( addr, IntFmt<hex,fixed>() ) );

          ValVector<uint32_t> result = client->readBlock ( addr, blockSize, defs::INCREMENTAL );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedBlockRead ( addr, result, registers.begin() + ( addr - aBaseAddr ) ) ) );
          nrQueuedWords += blockSize;
          break;
        }
        case 1: // write
        {
          blockSize = getRandomBlockSize ( aBaseAddr + aDepth - addr );
          log ( Notice(), "Soak test - queueing: ", Integer ( blockSize ), "-word write at ", Integer ( addr, IntFmt<hex,fixed>() ) );

          vector<uint32_t> randomData = getRandomBuffer ( blockSize );
          ValHeader result = client->writeBlock ( addr, randomData, defs::INCREMENTAL );
          std::copy ( randomData.begin(), randomData.end(), registers.begin() + ( addr - aBaseAddr ) );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedBlockWrite ( addr, blockSize, result ) ) );
          nrQueuedWords += blockSize;
          break;
        }
        case 2: // RMW-bits
        {
          log ( Notice(), "Soak test - queueing: RMW-bits at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = rand();
          tempUInt2 = rand();
          vector<uint32_t>::iterator regIt = registers.begin() + ( addr - aBaseAddr );

          if ( ipbus_vsn == 1 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          ValWord<uint32_t> result = client->rmw_bits ( addr, tempUInt1, tempUInt2 );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedRmwBits ( addr, tempUInt1, tempUInt2, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( ipbus_vsn == 2 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          break;
        }
        case 3: // RMW-sum
        {
          log ( Notice(), "Soak test - queueing: RMW-sum at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = rand();
          vector<uint32_t>::iterator regIt = registers.begin() + ( addr - aBaseAddr );

          if ( ipbus_vsn == 1 )
          {
            *regIt += tempUInt1;
          }

          ValWord<uint32_t> result = client->rmw_sum ( addr, tempUInt1 );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedRmwSum ( addr, tempUInt1, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( ipbus_vsn == 2 )
          {
            *regIt += tempUInt1;
          }

          break;
        }
      }

      if ( aDispatchEachIteration || ( nrQueuedWords > 20000000 ) || ( i == aNrIterations ) )
      {
        log ( Notice(), "Soak test - issuing dispatch" );
        client->dispatch();
        log ( Notice(), "Soak test - issuing empty dispatch" );
        client->dispatch();

        for ( vector< boost::shared_ptr<QueuedTransaction> >::const_iterator it = queuedTransactions.begin(); it != queuedTransactions.end(); it++ )
        {
          if ( ! ( *it )->check_values() )
          {
            cout << "ERROR OCCURED IN SOAK TEST to '" << client->uri() << "' - after " << i << " successful transactions" << endl;
            return false;
          }
        }

        queuedTransactions.clear();
        nrQueuedWords = 0;

        if ( ! aVerbose )
        {
          std::cout << "No errors after " << i << " transactions -- " << setiosflags ( ios::fixed ) << setprecision ( 1 ) << ( 100.0 * i ) / aNrIterations << "% done\r";
          std::cout.flush();
        }
      }
    }
  }//end: for, aClients

  cout << endl << "Reached end of soak testing successfully!" << endl;
  return true;
}


// PerfTester::QueuedBlockRead MEMBER FUNCTIONS

uhal::tests::PerfTester::QueuedBlockRead::QueuedBlockRead ( const uint32_t addr, const ValVector<uint32_t>& valVector, std::vector<uint32_t>::const_iterator expectedValuesIt ) :
  m_depth ( valVector.size() ),
  m_addr ( addr ),
  m_valVector ( valVector )
{
  m_expected.assign ( expectedValuesIt, expectedValuesIt + m_depth );
}

uhal::tests::PerfTester::QueuedBlockRead::~QueuedBlockRead()
{ }

bool uhal::tests::PerfTester::QueuedBlockRead::check_values()
{
  std::vector<uint32_t>::const_iterator valVecIt = m_valVector.begin();
  std::vector<uint32_t>::const_iterator expdIt = m_expected.begin();

  for ( ; valVecIt != m_valVector.end(); valVecIt++, expdIt++ )
  {
    if ( ( *valVecIt ) != ( *expdIt ) )
    {
      uint32_t addr = m_addr + ( valVecIt - m_valVector.begin() );
      log ( Error(), "TEST FAILED: In ", Integer ( m_depth ), "-word read @ ", Integer ( m_addr, IntFmt<hex,fixed>() ), ", register ", Integer ( addr, IntFmt<hex,fixed>() ), " has value ", Integer ( *valVecIt, IntFmt<hex,fixed>() ), ", but expected value ", Integer ( *expdIt, IntFmt<hex,fixed>() ) );
      return false;
    }
  }

  log ( Notice(), "TEST PASSED: Incrementing ", Integer ( m_depth ), "-word read @ ", Integer ( m_addr, IntFmt<hex,fixed>() ), " --> ", Integer ( m_addr + m_depth - 1, IntFmt<hex,fixed>() ) );
  return true;
}


// PerfTester::QueuedBlockWrite MEMBER FUNCTIONS

uhal::tests::PerfTester::QueuedBlockWrite::QueuedBlockWrite ( const uint32_t addr, const uint32_t depth, const ValHeader& valHeader ) :
  m_depth ( depth ),
  m_addr ( addr ),
  m_valHeader ( valHeader )
{ }

uhal::tests::PerfTester::QueuedBlockWrite::~QueuedBlockWrite()
{ }

bool uhal::tests::PerfTester::QueuedBlockWrite::check_values()
{
  if ( ! m_valHeader.valid() )
  {
    log ( Error(), "TEST FAILED: Incrementing ", Integer ( m_depth ), "-word write @ ", Integer ( m_addr, IntFmt<hex,fixed>() ), " unsuccessful." );
    return false;
  }

  log ( Notice(), "TEST PASSED: Incrementing ", Integer ( m_depth ), "-word write @ ", Integer ( m_addr, IntFmt<hex,fixed>() ), " --> ", Integer ( m_addr + m_depth - 1, IntFmt<hex,fixed>() ) );
  return true;
}


// PerfTester::QueuedRmwBits MEMBER FUNCTIONS

uhal::tests::PerfTester::QueuedRmwBits::QueuedRmwBits ( const uint32_t addr, const uint32_t a, const uint32_t b, const ValWord<uint32_t>& valWord, const uint32_t expected ) :
  m_addr ( addr ),
  m_and ( a ),
  m_or ( b ),
  m_valWord ( valWord ),
  m_expected ( expected )
{ }

uhal::tests::PerfTester::QueuedRmwBits::~QueuedRmwBits()
{ }

bool uhal::tests::PerfTester::QueuedRmwBits::check_values()
{
  if ( m_valWord.value() != m_expected )
  {
    log ( Error(), "TEST FAILED: RMW-bits @ ", Integer ( m_addr, IntFmt<hex,fixed>() ), " (AND=", Integer ( m_and, IntFmt<hex,fixed>() ), ", OR=", Integer ( m_or, IntFmt<hex,fixed>() ), "). Transaction returned ", Integer ( m_valWord.value(), IntFmt<hex,fixed>() ), ", but expected ", Integer ( m_expected, IntFmt<hex,fixed>() ) );
    return false;
  }

  log ( Notice(), "TEST PASSED: RMW-bits @ ", Integer ( m_addr, IntFmt<hex,fixed>() ) );
  return true;
}


// PerfTester::QueuedRmwSum MEMBER FUNCTIONS

uhal::tests::PerfTester::QueuedRmwSum::QueuedRmwSum ( const uint32_t addr, const uint32_t a, const ValWord<uint32_t>& valWord, const uint32_t expected ) :
  m_addr ( addr ),
  m_addend ( a ),
  m_valWord ( valWord ),
  m_expected ( expected )
{ }

uhal::tests::PerfTester::QueuedRmwSum::~QueuedRmwSum()
{ }

bool uhal::tests::PerfTester::QueuedRmwSum::check_values()
{
  if ( m_valWord.value() != m_expected )
  {
    log ( Error(), "TEST FAILED: RMW-sum @ ", Integer ( m_addr, IntFmt<hex,fixed>() ), ", ADDEND=", Integer ( m_addend, IntFmt<hex,fixed>() ), ". Transaction returned ", Integer ( m_valWord.value(), IntFmt<hex,fixed>() ), ", but I expected ", Integer ( m_expected, IntFmt<>() ) );
    return false;
  }

  log ( Notice(), "TEST PASSED: RMW-sum @ ", Integer ( m_addr, IntFmt<hex,fixed>() ) );
  return true;
}
