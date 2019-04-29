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


namespace uhal {
namespace tests {


uint32_t PerfTester::getRandomBlockSize ( const uint32_t maxSize )
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


template <>
std::vector<uint32_t> PerfTester::getRandomBuffer<uint32_t>( unsigned size )
{
  std::vector<uint32_t> buffer;
  buffer.reserve ( size );

  // Never said anything about it being a flat random distribution... ;-)
  for ( unsigned i = 0 ; i < size ; ++i )
  {
    buffer.push_back ( rand() + rand() );
  }

  return buffer;
}


template<>
std::vector<uint64_t> PerfTester::getRandomBuffer<uint64_t>( unsigned size )
{
  std::vector<uint64_t> buffer;
  buffer.reserve ( size );

  // Never said anything about it being a flat random distribution... ;-)
  for ( unsigned i = 0 ; i < size ; ++i )
  {
    buffer.push_back ( ( uint64_t(rand() + rand()) << 32 ) | uint64_t(rand() + rand()) );
  }

  return buffer;
}


// Validation test -- single-register write/read-back
bool PerfTester::validation_test_single_write_read ( ClientInterface& c, const uint32_t addr, const bool perTransactionDispatch, const bool aVerbose )
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
bool PerfTester::validation_test_block_write_read ( ClientInterface& c, const uint32_t addr, const uint32_t depth, const bool perTransactionDispatch, const bool aVerbose )
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

  const std::vector<uint32_t> xx = getRandomBuffer<uint32_t>( depth );

  try
  {
    c.writeBlock ( addr, xx );

    if ( perTransactionDispatch )
    {
      c.dispatch();
    }

    uhal::ValVector<uint32_t> ram = c.readBlock ( addr, depth );
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
bool PerfTester::validation_test_write_rmwbits_read ( ClientInterface& c, const uint32_t addr, const bool perTransactionDispatch, const bool aVerbose )
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

  const bool ipbus2 = ( ( c.uri().find ( "ipbusudp-2.0" ) != string::npos ) || ( c.uri().find ( "ipbustcp-2.0" ) != string::npos ) || ( c.uri().find ( "chtcp-2.0" ) != string::npos ) || ( c.uri().find ( "ipbuspcie-2.0" ) != string::npos ) || ( c.uri().find ( "ipbusmmap-2.0" ) != string::npos ) || ( c.uri().find ( "ipbusudp-3.0" ) != string::npos ) || ( c.uri().find ( "ipbuspcie-3.0" ) != string::npos ) );

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
bool PerfTester::validation_test_write_rmwsum_read ( ClientInterface& c, const uint32_t addr, const bool perTransactionDispatch, const bool aVerbose )
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

  const bool ipbus2 = ( ( c.uri().find ( "ipbusudp-2.0" ) != string::npos ) || ( c.uri().find ( "ipbustcp-2.0" ) != string::npos ) || ( c.uri().find ( "chtcp-2.0" ) != string::npos )  || ( c.uri().find ( "ipbuspcie-2.0" ) != string::npos ) || ( c.uri().find ( "ipbusmmap-2.0" ) != string::npos ) || ( c.uri().find ( "ipbusudp-3.0" ) != string::npos ) || ( c.uri().find ( "ipbuspcie-3.0" ) != string::npos ) );

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

uint8_t PerfTester::getIPbusMajorVsn(const std::string& aURI)
{
  if ( aURI.find ( "-1.3" ) != std::string::npos )
  {
    return 1;
  }
  else if ( aURI.find ( "-2.0" ) != std::string::npos )
  {
    return 2;
  }
  else if ( aURI.find ( "-3.0" ) != std::string::npos )
  {
    return 3;
  }
  else
  {
    throw std::runtime_error("Cannot deduce protocol from URI \"" + aURI + "\"" );
  }
}


bool PerfTester::runValidationTest(const std::vector<ClientInterface*>& aClients, const uint32_t aBaseAddr, const uint32_t aDepth, const size_t aNrIterations, const bool aDispatchEachIteration, const bool aVerbose)
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
        std::exit(1);
      }

      nrTestsTotal++;

      if ( ! validation_test_single_write_read ( *client, addr_vec.at ( i ), false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
        std::exit(1);
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
        std::exit(1);
      }

      nrTestsTotal++;

      if ( ! validation_test_block_write_read ( *client, addr, depth, false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
        std::exit(1);
      }
    }

    cout << "\n 3. Testing RMW-bits (write, RMWbits, read; " << addr_vec.size() * 2 << " tests)" << endl;

    for ( std::vector<uint32_t>::const_iterator it = addr_vec.begin(); it != addr_vec.end(); it++ )
    {
      nrTestsTotal++;

      if ( ! validation_test_write_rmwbits_read ( *client, *it, true, aVerbose ) )
      {
        nrTestsFailed++;
        std::exit(1);
      }

      nrTestsTotal++;

      if ( ! validation_test_write_rmwbits_read ( *client, *it, false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
        std::exit(1);
      }
    }

    cout << "\n 4. Testing RMW-sum (write, RMW-sum, read; " << addr_vec.size() * 2 << " tests)" << endl;

    for ( std::vector<uint32_t>::const_iterator it = addr_vec.begin(); it != addr_vec.end(); it++ )
    {
      nrTestsTotal++;

      if ( ! validation_test_write_rmwsum_read ( *client, *it, true, aVerbose ) )
      {
        nrTestsFailed++;
        std::exit(1);
      }

      nrTestsTotal++;

      if ( ! validation_test_write_rmwsum_read ( *client, *it, false || aDispatchEachIteration, aVerbose ) )
      {
        nrTestsFailed++;
        std::exit(1);
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
    uint8_t lIPbusMajorVsn = getIPbusMajorVsn(client->uri());


    // Initialise registers to 0x0
    std::vector< uint64_t > registers ( aDepth , 0x00000000 );
    client->writeBlock64 ( aBaseAddr, registers );
    client->dispatch();
    // ACTUAL MEAT OF SOAK TEST
    uint32_t type, addr, blockSize;
    uint64_t tempUInt1, tempUInt2;
    vector< boost::shared_ptr<QueuedTransaction> > queuedTransactions;
    uint32_t nrQueuedWords = 0;

    for ( unsigned i = 1; i <= aNrIterations; i++ )
    {
      type = ( rand() % 8 );
      addr = aBaseAddr + ( rand() % aDepth );

      switch ( type )
      {
        case 0: // read (32 bit data)
        {
          blockSize = getRandomBlockSize ( aBaseAddr + aDepth - addr );
          log ( Notice(), "Soak test - queueing: ", Integer ( blockSize ), "-word read at ", Integer ( addr, IntFmt<hex,fixed>() ) );

          ValVector<uint32_t> result = client->readBlock ( addr, blockSize, defs::INCREMENTAL );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedBlockRead<uint32_t>( addr, result, registers.begin() + ( addr - aBaseAddr ) ) ) );
          nrQueuedWords += blockSize;
          break;
        }
        case 1: // read (64 bit data)
        {
          blockSize = getRandomBlockSize ( aBaseAddr + aDepth - addr );
          log ( Notice(), "Soak test - queueing: ", Integer ( blockSize ), "-word 64-bit read at ", Integer ( addr, IntFmt<hex,fixed>() ) );

          ValVector<uint64_t> result = client->readBlock64 ( addr, blockSize, defs::INCREMENTAL );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedBlockRead<uint64_t>( addr, result, registers.begin() + ( addr - aBaseAddr ) ) ) );
          nrQueuedWords += blockSize;
          break;
        }
        case 2: // write (32 bit data)
        {
          blockSize = getRandomBlockSize ( aBaseAddr + aDepth - addr );
          log ( Notice(), "Soak test - queueing: ", Integer ( blockSize ), "-word write at ", Integer ( addr, IntFmt<hex,fixed>() ) );

          vector<uint32_t> randomData = getRandomBuffer<uint32_t>( blockSize );
          ValHeader result = client->writeBlock ( addr, randomData, defs::INCREMENTAL );
          for (size_t i=0; i < blockSize; i++)
            registers.at(addr + i - aBaseAddr) = (registers.at(addr + i - aBaseAddr) & uint64_t(0xFFFFFFFF00000000)) | uint64_t(randomData.at(i));
          // std::copy ( randomData.begin(), randomData.end(), registers.begin() + ( addr - aBaseAddr ) );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedBlockWrite<uint32_t>( addr, blockSize, result ) ) );
          nrQueuedWords += blockSize;
          break;
        }
        case 3: // write (64 bit data)
        {
          blockSize = getRandomBlockSize ( aBaseAddr + aDepth - addr );
          log ( Notice(), "Soak test - queueing: ", Integer ( blockSize ), "-word 64-bit write at ", Integer ( addr, IntFmt<hex,fixed>() ) );

          vector<uint64_t> randomData = getRandomBuffer<uint64_t>( blockSize );
          ValHeader result = client->writeBlock64 ( addr, randomData, defs::INCREMENTAL );
          std::copy ( randomData.begin(), randomData.end(), registers.begin() + ( addr - aBaseAddr ) );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedBlockWrite<uint64_t>( addr, blockSize, result ) ) );
          nrQueuedWords += blockSize;
          break;
        }
        case 4: // RMW-bits (32 bit data)
        {
          log ( Notice(), "Soak test - queueing: RMW-bits at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = (uint64_t(0xFFFFFFFF) << 32) | uint64_t(rand());
          tempUInt2 = rand();
          vector<uint64_t>::iterator regIt = registers.begin() + ( addr - aBaseAddr );

          if ( lIPbusMajorVsn == 1 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          ValWord<uint32_t> result = client->rmw_bits ( addr, tempUInt1, tempUInt2 );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedRmwBits<uint32_t> ( addr, tempUInt1, tempUInt2, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( lIPbusMajorVsn != 1 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          break;
        }
        case 5: // RMW-bits (64 bit data)
        {
          log ( Notice(), "Soak test - queueing: 64-bit RMW-bits at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = (uint64_t(rand()) << 32) | rand();
          tempUInt2 = (uint64_t(rand()) << 32) | rand();
          vector<uint64_t>::iterator regIt = registers.begin() + ( addr - aBaseAddr );

          if ( lIPbusMajorVsn == 1 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          ValWord<uint64_t> result = client->rmw_bits64 ( addr, tempUInt1, tempUInt2 );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedRmwBits<uint64_t> ( addr, tempUInt1, tempUInt2, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( lIPbusMajorVsn != 1 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          break;
        }
        case 6: // RMW-sum (32 bit data)
        {
          log ( Notice(), "Soak test - queueing: RMW-sum at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = rand();
          vector<uint64_t>::iterator regIt = registers.begin() + ( addr - aBaseAddr );

          if ( lIPbusMajorVsn == 1 )
          {
            *regIt = (*regIt & uint64_t(0xFFFFFFFF00000000)) | uint32_t(uint32_t(*regIt) + uint32_t(tempUInt1));
          }

          ValWord<uint32_t> result = client->rmw_sum ( addr, tempUInt1 );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedRmwSum<uint32_t> ( addr, tempUInt1, result, uint32_t(*regIt) ) ) );
          nrQueuedWords += 1;

          if ( lIPbusMajorVsn != 1 )
          {
            *regIt = (*regIt & uint64_t(0xFFFFFFFF00000000)) | uint32_t(uint32_t(*regIt) + uint32_t(tempUInt1));
          }

          break;
        }
        case 7: // RMW-sum (64 bit data)
        {
          log ( Notice(), "Soak test - queueing: 64-bit RMW-sum at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = (uint64_t(rand()) << 32) | rand();
          vector<uint64_t>::iterator regIt = registers.begin() + ( addr - aBaseAddr );

          if ( lIPbusMajorVsn == 1 )
          {
            *regIt += tempUInt1;
          }

          ValWord<uint64_t> result = client->rmw_sum64 ( addr, tempUInt1 );
          queuedTransactions.push_back ( boost::shared_ptr<QueuedTransaction> ( new QueuedRmwSum<uint64_t> ( addr, tempUInt1, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( lIPbusMajorVsn != 1 )
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
          if ( ! ( *it )->checkValues() )
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

template <typename T>
PerfTester::QueuedBlockRead<T>::QueuedBlockRead ( const uint32_t aAddr, const ValVector<T>& aValVector, std::vector<uint64_t>::const_iterator aExpectedBeginIt ) :
  mAddr ( aAddr ),
  mValVector ( aValVector )
{
  mExpected.assign ( aExpectedBeginIt, aExpectedBeginIt + aValVector.size() );
}

template <typename T>
PerfTester::QueuedBlockRead<T>::~QueuedBlockRead()
{
}

template <typename T>
bool PerfTester::QueuedBlockRead<T>::checkValues()
{
  typename std::vector<T>::const_iterator lValVecIt = mValVector.begin();
  typename std::vector<T>::const_iterator lExpdIt = mExpected.begin();

  for ( ; lValVecIt != mValVector.end(); lValVecIt++, lExpdIt++ )
  {
    if ( ( *lValVecIt ) != ( *lExpdIt ) )
    {
      uint32_t lAddr = mAddr + ( lValVecIt - mValVector.begin() );
      log ( Error(), "TEST FAILED: In ", Integer ( mValVector.size() ), "-word read @ ", Integer ( mAddr, IntFmt<hex,fixed>() ), ", register ", Integer ( mAddr, IntFmt<hex,fixed>() ), " has value ", Integer ( *lValVecIt, IntFmt<hex,fixed>() ), ", but expected value ", Integer ( *lExpdIt, IntFmt<hex,fixed>() ) );
      return false;
    }
  }

  log ( Notice(), "TEST PASSED: Incrementing ", Integer ( mValVector.size() ), "-word read @ ", Integer ( mAddr, IntFmt<hex,fixed>() ), " --> ", Integer ( uint32_t(mAddr + mValVector.size() - 1), IntFmt<hex,fixed>() ) );
  return true;
}


// PerfTester::QueuedBlockWrite MEMBER FUNCTIONS

template <typename T>
PerfTester::QueuedBlockWrite<T>::QueuedBlockWrite ( const uint32_t aAddr, const size_t aDepth, const ValHeader& aValHeader ) :
  mDepth ( aDepth ),
  mAddr ( aAddr ),
  mValHeader ( aValHeader )
{ }

template <typename T>
PerfTester::QueuedBlockWrite<T>::~QueuedBlockWrite()
{ }

template <typename T>
bool PerfTester::QueuedBlockWrite<T>::checkValues()
{
  if ( ! mValHeader.valid() )
  {
    log ( Error(), "TEST FAILED: Incrementing ", Integer ( mDepth ), "-word write @ ", Integer ( mAddr, IntFmt<hex,fixed>() ), " unsuccessful." );
    return false;
  }

  log ( Notice(), "TEST PASSED: Incrementing ", Integer ( mDepth ), "-word write @ ", Integer ( mAddr, IntFmt<hex,fixed>() ), " --> ", Integer ( uint32_t(mAddr + mDepth - 1), IntFmt<hex,fixed>() ) );
  return true;
}


// PerfTester::QueuedRmwBits MEMBER FUNCTIONS

template <typename T>
PerfTester::QueuedRmwBits<T>::QueuedRmwBits ( const uint32_t aAddr, const T a, const T b, const ValWord<T>& aValWord, const T aExpected ) :
  mAddr ( aAddr ),
  mAnd ( a ),
  mOr ( b ),
  mValWord ( aValWord ),
  mExpected ( aExpected )
{ }

template <typename T>
PerfTester::QueuedRmwBits<T>::~QueuedRmwBits()
{ }

template <typename T>
bool PerfTester::QueuedRmwBits<T>::checkValues()
{
  if ( mValWord.value() != mExpected )
  {
    log ( Error(), "TEST FAILED: RMW-bits @ ", Integer ( mAddr, IntFmt<hex,fixed>() ), " (AND=", Integer ( mAnd, IntFmt<hex,fixed>() ), ", OR=", Integer ( mOr, IntFmt<hex,fixed>() ), "). Transaction returned ", Integer ( mValWord.value(), IntFmt<hex,fixed>() ), ", but expected ", Integer ( mExpected, IntFmt<hex,fixed>() ) );
    return false;
  }

  log ( Notice(), "TEST PASSED: RMW-bits @ ", Integer ( mAddr, IntFmt<hex,fixed>() ) );
  return true;
}


// PerfTester::QueuedRmwSum MEMBER FUNCTIONS

template <typename T>
PerfTester::QueuedRmwSum<T>::QueuedRmwSum ( const uint32_t aAddr, const T aAddend, const ValWord<T>& aValWord, const T aExpected ) :
  mAddr ( aAddr ),
  mAddend ( aAddend ),
  mValWord ( aValWord ),
  mExpected ( aExpected )
{ }

template <typename T>
PerfTester::QueuedRmwSum<T>::~QueuedRmwSum()
{ }

template <typename T>
bool PerfTester::QueuedRmwSum<T>::checkValues()
{
  if ( mValWord.value() != mExpected )
  {
    log ( Error(), "TEST FAILED: RMW-sum @ ", Integer ( mAddr, IntFmt<hex,fixed>() ), ", ADDEND=", Integer ( mAddend, IntFmt<hex,fixed>() ), ". Transaction returned ", Integer ( mValWord.value(), IntFmt<hex,fixed>() ), ", but I expected ", Integer ( mExpected, IntFmt<hex,fixed>() ) );
    return false;
  }

  log ( Notice(), "TEST PASSED: RMW-sum @ ", Integer ( mAddr, IntFmt<hex,fixed>() ) );
  return true;
}


} // end ns tests
} // end ns uhal
