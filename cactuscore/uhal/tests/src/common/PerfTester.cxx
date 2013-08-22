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


// Namespace resolution
namespace po = boost::program_options;
using namespace std;


// PUBLIC METHODS

uhal::tests::PerfTester::PerfTester() :
  m_testDescMap(),
  m_testFuncMap(),
  m_deviceURIs(),
  m_clients(),
  m_testName ( "BandwidthRx" ),
  m_iterations ( 1000 ),
  m_baseAddrStr ( "0x0" ),
  m_baseAddr ( 0 ),
  m_bandwidthTestDepth ( 0 ),
  m_verbose ( false ),
  m_perIterationDispatch ( false )
{
  // ***** DECLARE TESTS HERE - descriptions should not be longer than a shortish line. *****:
  // Receive bandwidth test
  m_testFuncMap["BandwidthRx"] = &PerfTester::bandwidthRxTest;
  m_testDescMap["BandwidthRx"] = "Block read test (default depth = 340) to find the receive bandwidth.";
  // Transmit bandwidth test
  m_testFuncMap["BandwidthTx"] = &PerfTester::bandwidthTxTest;
  m_testDescMap["BandwidthTx"] = "Block write test (default depth = 340) to find the transmit bandwidth.";
  // Validation test
  m_testFuncMap["Validation"] = &PerfTester::validationTest;
  m_testDescMap["Validation"] = "For validating downstream subsystems, such as the Control Hub or the IPbus firmware.";
  // Sandbox test
  m_testFuncMap["Sandbox"] = &PerfTester::sandbox;
  m_testDescMap["Sandbox"] = "A user-definable test - modify the sandbox() function to whatever you wish.";
  // ***** END DECLARATION OF TESTS *****
  // Initialise random number generator
  srand ( time ( NULL ) );
}


int uhal::tests::PerfTester::run ( int argc, char* argv[] )
{
  try
  {
    // This just defines and parses the command line parameters that are allowed.
    po::options_description argDescriptions ( "Allowed options" );
    argDescriptions.add_options()
    ( "help,h", "Produce help message" )
    ( "list,l", "List the names of the different tests that can be performed." )
    ( "verbose,v", "Make the output more verbose." )
    ( "test,t", po::value<string> ( &m_testName )->default_value ( "BandwidthRx" ), "Name of the test to be performed." )
    ( "iterations,i", po::value<unsigned> ( &m_iterations )->default_value ( 1000 ), "Number of test iterations to run." )
    ( "devices,d", po::value<StringVec> ( &m_deviceURIs )->multitoken(), "List of device connection URIs, e.g. chtcp-1.3://..., etc" )
    ( "baseAddr,b", po::value<string> ( &m_baseAddrStr )->default_value ( "0x0" ), "Base address (in hex) of the test location on the target device(s)." )
    ( "bandwidthTestDepth,w", po::value<boost::uint32_t> ( &m_bandwidthTestDepth )->default_value ( 340 ), "Depth of read/write used in bandwidth tests." )
    ( "perIterationDispatch,p", "Force a network dispatch every test iteration instead of the default single dispatch call at the end." );
    po::variables_map argMap;
    po::store ( po::parse_command_line ( argc, argv, argDescriptions ), argMap );
    po::notify ( argMap );
    // Convert the hexadecimal base addr value into an actual number.
    // This is just a workaround for boost:program_options not understanding hex input
    istringstream convert ( m_baseAddrStr );
    convert >> std::hex >> m_baseAddr;

    if ( convert.fail() )
    {
      cerr << "The specified base address was not valid hexadecimal!" << endl;
      return 10;
    }

    // Display help info and exit
    if ( argc < 2 || argMap.count ( "help" ) )
    {
      ostringstream oss;
      oss << argDescriptions;
      outputHelpText ( oss.str() );
      return 20;
    }

    if ( argMap.count ( "list" ) )
    {
      outputTestDescriptionsList();
      return 30;
    }

    if ( argMap.count ( "verbose" ) )
    {
      m_verbose = true;
    }

    if ( argMap.count ( "perIterationDispatch" ) )
    {
      m_perIterationDispatch = true;
    }

    if ( badInput() )
    {
      return 40;    // Report bad user input and exit if necessary.
    }

    outputUserChoices();  // Echo back to the user the settings they have selected.
    buildClients(); // Build the clients from the device URIs provided by the user
    ( this->*m_testFuncMap.find ( m_testName )->second ) (); // Calls the test function, based on the test name.
  }
  catch ( std::exception& e )
  {
    cerr << "Error:  " << e.what() << endl;
    return 50;
  }
  catch ( ... )
  {
    cerr << "Caught exception of unknown type!" << endl;
    return 60;
  }

  return 0;  // The program ran successfully (even if the test didn't...)
}


// PRIVATE METHODS - Test infrastructure

void uhal::tests::PerfTester::outputHelpText ( const string& argDescriptions ) const
{
  cout <<  "\n         -----------------------------------------\n"
       "         PerfTester.exe - IPbus Performance Tester\n"
       "         -----------------------------------------\n\n"
       "   Generate custom IPbus/uHAL tests from the command line\n\n"
       <<  argDescriptions
       <<  "Usage examples:\n\n"
       "  PerfTester.exe -t BandwidthTx -b 0xf0 -d ipbusudp-1.3://localhost:50001 ipbusudp-1.3://localhost:50002\n"
       "  PerfTester.exe -t BandwidthTx -w 5 -i 100 chtcp-1.3://localhost:10203?target=127.0.0.1:50001" << endl;
  outputTestDescriptionsList();
}


void uhal::tests::PerfTester::outputTestDescriptionsList() const
{
  cout << "\nNames and descriptions of available tests:\n" << endl;
  cout << "  " << setw ( 16 ) << left << "Name" << "  " << "Description" << endl;
  cout << "  " << setw ( 16 ) << left << "----" << "  " << "-----------" << endl;
  TestDescMap::const_iterator iTest = m_testDescMap.begin(), iTestEnd = m_testDescMap.end();

  for ( ; iTest != iTestEnd ; ++iTest )
  {
    cout << "  " << setw ( 16 ) << left << iTest->first << "  "  << iTest->second << endl;
  }

  cout << endl;
}


bool uhal::tests::PerfTester::badInput() const
{
  if ( m_deviceURIs.empty() )
  {
    cerr << "You must specify at least one device connection URI by using the -d option!" << endl;
    return true;
  }

  if ( m_testFuncMap.find ( m_testName ) == m_testFuncMap.end() )
  {
    cerr << "The test name '" << m_testName
         << "' is not one of the available tests!\nDo 'PerfTester -l' to see names of available tests!" << endl;
    return true;
  }

  return false;
}


void uhal::tests::PerfTester::outputUserChoices() const
{
  cout << "Test settings:\n" << endl
       << "  Test Name  -------------->  " << m_testName << endl
       << "  Test register addr  ----->  " << std::hex << showbase << m_baseAddr << noshowbase << std::dec << endl
       << "  Test iterations  -------->  " << m_iterations << endl
       << "  Per-iteration dispatch -->  " << ( m_perIterationDispatch?"Yes":"No" ) << endl
       << "  Device URIs:" << endl;
  StringVec::const_iterator iDevice = m_deviceURIs.begin(), iDeviceEnd = m_deviceURIs.end();

  for ( ; iDevice != iDeviceEnd ; ++iDevice )
  {
    cout << "    " << *iDevice << endl;
  }

  cout << "\nRunning test now...\n" << endl;
}


void uhal::tests::PerfTester::buildClients()
{
  if ( m_verbose )
  {
    setLogLevelTo ( Debug() );
    cout << "Building device clients..." << endl;
  }
  else
  {
    setLogLevelTo ( Warning() );
  }

  m_clients.reserve ( m_deviceURIs.size() );

  for ( unsigned int iURI = 0 ; iURI < m_deviceURIs.size() ; ++iURI )
  {
    m_clients.push_back ( ClientFactory::getInstance().getClient ( "MyDevice", m_deviceURIs.at ( iURI ) ) );
  }

  if ( m_verbose )
  {
    cout << "Device clients built successfully!" << endl;
  }
}


void uhal::tests::PerfTester::outputStandardResults ( double totalSeconds ) const
{
  string underline;
  underline.assign ( m_testName.size() + 14, '-' );
  cout << m_testName << " Test Results:\n"
       << underline << "\n\n"
       << "Number of IPbus hosts in test   = " << m_deviceURIs.size() << "\n"
       << "Total test iterations           = " << m_iterations << "\n"
       << "Total time taken                = " << totalSeconds << " s\n"
       << "Test iteration frequency        = " << m_iterations/totalSeconds << " Hz" << endl;
}


uhal::tests::PerfTester::U32Vec uhal::tests::PerfTester::getRandomBuffer ( unsigned size ) const
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


bool uhal::tests::PerfTester::buffersEqual ( const U32Vec& writeBuffer, const U32ValVec& readBuffer ) const
{
  return std::equal ( readBuffer.begin(), readBuffer.end(), writeBuffer.begin() );
}


// Validation test -- single-register write/read-back
bool uhal::tests::PerfTester::validation_test_single_write_read(ClientPtr& c, const uint32_t addr, const bool perTransactionDispatch) const
{
  std::ostringstream oss_details;
  oss_details << "Single-register write-read @ 0x" << std::hex << addr << (perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );
  if( m_verbose )
    cout << oss_details.str() << endl;

  const uint32_t x = rand();

  try
  {
    c->write(addr, x);
    if( perTransactionDispatch )
      c->dispatch();
    
    ValWord<uint32_t> reg = c->read(addr);
    c->dispatch();

    if (x != reg.value())
    {
      cout << "TEST FAILED: " << oss_details.str() << ". Wrote value 0x" << std::hex << x << " but read-back 0x" << reg.value() << std::dec << endl;
      return false;
    }
  }
  catch (const std::exception& e)
  {
    cout << "TEST FAILED: " << oss_details.str() << ". Exception of type '" << typeid(e).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }
  return true;
}


// Validation test -- block write/read-back
bool uhal::tests::PerfTester::validation_test_block_write_read(ClientPtr& c, const uint32_t addr, const uint32_t depth, const bool perTransactionDispatch) const
{
  std::ostringstream oss_details; 
  oss_details << depth << "-word write-read @ 0x" << std::hex << addr;
  if ( depth>1 )
    oss_details << " to 0x" << addr+depth-1 << std::dec;
  oss_details << (perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if( m_verbose )
    cout << "Running test: " << oss_details.str() << endl;

  const U32Vec xx = getRandomBuffer(depth);

  try
  {
    c->writeBlock(addr, xx);
    if( perTransactionDispatch )
      c->dispatch();

    U32ValVec ram = c->readBlock(addr, depth);
    c->dispatch();

    std::vector<uint32_t>::const_iterator valVecIt = ram.begin();
    std::vector<uint32_t>::const_iterator xxIt = xx.begin();
    for(; valVecIt != ram.end(); valVecIt++, xxIt++)
    {
      if ( (*valVecIt) != (*xxIt) )
      {
        uint32_t reg_addr = addr + (valVecIt - ram.begin());
        cout << "TEST FAILED: " << oss_details.str() << ". Wrote value Ox" << std::hex << *xxIt << " to register 0x" << reg_addr << " but read-back 0x" << *valVecIt << std::dec << std::endl;
        return false;
      }
    }

    log ( Notice(), "TEST PASSED: ", oss_details.str() );
  }
  catch (const std::exception& e)
  {
    cout << "TEST FAILED: " << oss_details.str() << "! EXCEPTION of type '" << typeid(e).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }
  return true;
}


// Validation test -- write, RMW bits, read
bool uhal::tests::PerfTester::validation_test_write_rmwbits_read(ClientPtr& c, const uint32_t addr, const bool perTransactionDispatch) const
{
  std::ostringstream oss_details;
  oss_details << "RMW-bits @ 0x" << std::hex << addr << (perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if( m_verbose )
    cout << "TESTING: " << oss_details.str() << endl;
  const uint32_t x0 = rand();
  const uint32_t a = rand();
  const uint32_t b = rand();
  const uint32_t x1 = (x0 & a) | b;
  std::ostringstream oss_values;
  oss_values << "Wrote value 0x" << std::hex << x0 << ", then did RMW-bits with AND-term 0x" << a << ", OR-term 0x" << b;

  const bool ipbus2 = ( (c->uri().find("ipbusudp-2.0") != string::npos) || (c->uri().find("ipbustcp-2.0") != string::npos) || (c->uri().find("chtcp-2.0") != string::npos) );

  try
  {
    c->write(addr, x0);
    if( perTransactionDispatch )
      c->dispatch();

    ValWord<uint32_t> reg_rmw = c->rmw_bits(addr, a, b);
    c->dispatch();

    if ( ipbus2 ? (x0 != reg_rmw.value()) : (x1 != reg_rmw.value()) )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... RMW-bits returned value 0x" << std::hex << reg_rmw.value() << ", but expected 0x" << (ipbus2 ? x0 : x1) << std::dec << endl << oss_values.str() << endl;
      return false;
    }

    ValWord<uint32_t> reg_read = c->read(addr);
    c->dispatch();

    if( x1 != reg_read.value() )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... Read after RMW-bits returned value 0x" << std::hex << reg_rmw.value() << ", but expected 0x" << std::dec << x1 << endl << oss_values.str() << endl;
      return false;
    }
  }
  catch (const std::exception& e)
  {
    cout << "TEST FAILED: " << oss_values.str() << ". Exception of type '" << typeid(e).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }

  return true;
}

// Validation test -- write, RMW sum, read
bool uhal::tests::PerfTester::validation_test_write_rmwsum_read(ClientPtr& c, const uint32_t addr, const bool perTransactionDispatch) const
{
  std::ostringstream oss_details;
  oss_details << "RMW-sum @ 0x" << std::hex << addr << (perTransactionDispatch ? " (multiple dispatches)" : " (single dispatch)" );

  if( m_verbose )
    cout << "TESTING: " << oss_details.str() << endl;
  const uint32_t x0 = rand();
  const uint32_t a = rand();
  const uint32_t x1 = x0 + a;
  std::ostringstream oss_values;
  oss_values << "Wrote value 0x" << std::hex << x0 << ", then did RMW-sum with ADDEND 0x" << a;

  const bool ipbus2 = ( (c->uri().find("ipbusudp-2.0") != string::npos) || (c->uri().find("ipbustcp-2.0") != string::npos) || (c->uri().find("chtcp-2.0") != string::npos) );

  try
  {
    c->write(addr, x0);
    if( perTransactionDispatch )
      c->dispatch();

    ValWord<uint32_t> reg_sum = c->rmw_sum(addr, a);
    c->dispatch();

    if ( ipbus2 ? (x0 != reg_sum.value()) : (x1 != reg_sum.value()) )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... RMW-sum returned value 0x" << std::hex << reg_sum.value() << ", but expected 0x" << (ipbus2 ? x0 : x1) << std::dec << endl << oss_values.str() << endl;
      return false;
    }

    ValWord<uint32_t> reg_read = c->read(addr);
    c->dispatch();

    if( x1 != reg_read.value() )
    {
      cout << "TEST FAILED: " << oss_details.str() << " ... Read after RMW-sum returned value 0x" << std::hex << reg_sum.value() << ", but expected 0x" << x1 << std::dec << endl << oss_values.str() << endl;
      return false;
    }
  }
  catch (const std::exception& e)
  {
    cout << "TEST FAILED: " << oss_values.str() << ". Exception of type '" << typeid(e).name() << "' thrown ..." << endl << e.what() << endl;
    return false;
  }

  return true;
}


// PRIVATE MEMBER FUNCTIONS - IPbus test functions that users can run

void uhal::tests::PerfTester::bandwidthRxTest()
{
  Timer myTimer;

  for ( unsigned i = 0; i < m_iterations ; ++i )
  {
    if ( m_verbose )
    {
      cout << "Iteration " << i << endl;
    }

    BOOST_FOREACH ( ClientPtr& iClient, m_clients )
    {
      iClient->readBlock ( m_baseAddr, m_bandwidthTestDepth, defs::NON_INCREMENTAL );
    }

    if ( m_perIterationDispatch )
    {
      BOOST_FOREACH ( ClientPtr& iClient, m_clients )
      {
        iClient->dispatch();
      }
    }
  }

  if ( !m_perIterationDispatch )
  {
    BOOST_FOREACH ( ClientPtr& iClient, m_clients )
    {
      iClient->dispatch();
    }
  }

  double totalSeconds = myTimer.elapsedSeconds();
  double totalPayloadKB = m_deviceURIs.size() * m_iterations * m_bandwidthTestDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;
  outputStandardResults ( totalSeconds );
  cout << "Read depth used each iteration  = " << m_bandwidthTestDepth << " 32-bit words\n"
       << "Total IPbus payload received    = " << totalPayloadKB << " KB\n"
       << "Average read bandwidth          = " << dataRateKB_s << " KB/s" << endl;
}


void uhal::tests::PerfTester::bandwidthTxTest()
{
  // Send buffer - lots of "cafebabe" (in little-endian)
  U32Vec sendBuffer ( m_bandwidthTestDepth, 0xbebafeca );
  Timer myTimer;

  for ( unsigned i = 0; i < m_iterations ; ++i )
  {
    if ( m_verbose )
    {
      cout << "Iteration " << i << endl;
    }

    // Create the packet
    BOOST_FOREACH ( ClientPtr& iClient, m_clients )
    {
      iClient->writeBlock ( m_baseAddr, sendBuffer, defs::NON_INCREMENTAL );
    }

    if ( m_perIterationDispatch )
    {
      BOOST_FOREACH ( ClientPtr& iClient, m_clients )
      {
        iClient->dispatch();
      }
    }
  }

  if ( !m_perIterationDispatch )
  {
    BOOST_FOREACH ( ClientPtr& iClient, m_clients )
    {
      iClient->dispatch();
    }
  }

  double totalSeconds = myTimer.elapsedSeconds();
  double totalPayloadKB = m_deviceURIs.size() * m_iterations * m_bandwidthTestDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;
  outputStandardResults ( totalSeconds );
  cout << "Write depth used each iteration = " << m_bandwidthTestDepth << " 32-bit words\n"
       << "Total IPbus payload sent        = " << totalPayloadKB << " KB\n"
       << "Average write bandwidth         = " << dataRateKB_s << " KB/s" << endl;
}


void uhal::tests::PerfTester::validationTest()
{
  unsigned nrTestsFailed = 0;
  unsigned nrTestsTotal  = 0;

  assert(m_clients.size() == 1);
  ClientPtr client = m_clients.at(0);
  
  cout << "\nWRITE READ-BACK TESTS" << endl;
  vector<uint32_t> addr_vec;
  addr_vec.push_back(m_baseAddr);
  addr_vec.push_back(m_baseAddr + m_bandwidthTestDepth - 1);
  for ( unsigned i = 0; i < 48; i++ )
    addr_vec.push_back(m_baseAddr + (rand() % m_bandwidthTestDepth) );

  cout << "\n 1. Single-register write/read (" << addr_vec.size() * 2 << " tests)" << endl;
  for (unsigned i = 0; i < addr_vec.size(); i++)
  {
    nrTestsTotal++;
    if ( ! validation_test_single_write_read(client, addr_vec.at(i), true) )
    {
      nrTestsFailed++;
    }

    nrTestsTotal++;
    if ( ! validation_test_single_write_read(client, addr_vec.at(i), false || m_perIterationDispatch) )
    {
      nrTestsFailed++;
    }
  }
  
  cout << "\n 2. Block write/read (" << addr_vec.size() * 2 << " tests)" << endl;
  for (unsigned i = 0; i < addr_vec.size(); i++)
  {
    uint32_t addr = addr_vec.at(i);
    uint32_t max_depth = m_baseAddr + m_bandwidthTestDepth - addr;
    uint32_t depth = rand() % (max_depth + 1);
    // Remove 0-word write/read tests until bug solved -- https://svnweb.cern.ch/trac/cactus/ticket/343
    if ( depth == 0 )
       depth = max_depth;

    nrTestsTotal++;
    if ( ! validation_test_block_write_read(client, addr, depth, true) )
    {
      nrTestsFailed++;
    }

    nrTestsTotal++;
    if ( ! validation_test_block_write_read(client, addr, depth, false || m_perIterationDispatch) )
    {
      nrTestsFailed++;
    }
  }

  cout << "\n 3. Testing RMW-bits (write, RMWbits, read; " << addr_vec.size() * 2 << " tests)" << endl;
  for (std::vector<uint32_t>::const_iterator it = addr_vec.begin(); it != addr_vec.end(); it++)
  {
    nrTestsTotal++;
    if ( ! validation_test_write_rmwbits_read(client, *it, true) )
    {
      nrTestsFailed++;
    }

    nrTestsTotal++;
    if ( ! validation_test_write_rmwbits_read(client, *it, false || m_perIterationDispatch) )
    {
      nrTestsFailed++;
    }
  }

  cout << "\n 4. Testing RMW-sum (write, RMW-sum, read; " << addr_vec.size() * 2 << " tests)" << endl;
  for (std::vector<uint32_t>::const_iterator it = addr_vec.begin(); it != addr_vec.end(); it++)
  {
    nrTestsTotal++;
    if ( ! validation_test_write_rmwsum_read(client, *it, true) )
    {
      nrTestsFailed++;
    }

    nrTestsTotal++;
    if ( ! validation_test_write_rmwsum_read(client, *it, false || m_perIterationDispatch) )
    {
      nrTestsFailed++;
    }
  }


  if ( nrTestsFailed == 0 )
    cout << "\nBASIC TESTS SUMMARY: All " << nrTestsTotal << " tests passed!" << endl;
  else
  {
    cout << "\nBASIC TESTS SUMMARY: Total of " << nrTestsTotal << " tests run -- " << nrTestsFailed << " tests FAILED , " << (nrTestsTotal - nrTestsFailed) << " tests PASSED" << endl;
    return;
  }
  
  // SOAK TEST - SETUP

  cout << "\n\nSOAK TEST\n   Random sequence of " << m_iterations << " transactions sent to hardware" << endl << endl;

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
      throw 0;
    }
  }


  std::vector< uint32_t > registers ( m_bandwidthTestDepth , 0x00000000 );

  // Initialise registers to 0x0 
  client->writeBlock(m_baseAddr, registers);
  client->dispatch();

  // SOAK TEST - RUNNING IT

  uint32_t type, addr, blockSize;
  uint32_t tempUInt1, tempUInt2;
  vector< boost::shared_ptr<QueuedTransaction> > queuedTransactions;
  uint32_t nrQueuedWords = 0;

  for(unsigned i = 0; i < m_iterations; i++)
  {
    type = ( rand() % 4 );
    addr = m_baseAddr + ( rand() % m_bandwidthTestDepth );

    switch ( type )
    {
      case 0:
        {// read
          log ( Notice(), "Soak test - queueing: ", Integer (blockSize), "-word read at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          blockSize = ( rand() % (m_baseAddr + m_bandwidthTestDepth - addr) );
          if ( blockSize == 0 ) // Remove 0-word reads until bug fixed
            blockSize = 1;

          ValVector<uint32_t> result = client->readBlock ( addr, blockSize, defs::INCREMENTAL );
          queuedTransactions.push_back( boost::shared_ptr<QueuedTransaction>( new QueuedBlockRead( addr, result, registers.begin() + (addr - m_baseAddr) ) ) );
          nrQueuedWords += blockSize;

          break;
        }
      case 1:
        {// write
          log ( Notice(), "Soak test - queueing: ", Integer (blockSize), "-word write at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          blockSize = ( rand() % (m_baseAddr + m_bandwidthTestDepth - addr) );
          if ( blockSize == 0 ) // Remove 0-word writes until bug fixed
            blockSize = 1;

          vector<uint32_t> randomData = getRandomBuffer(blockSize);
          ValHeader result = client->writeBlock ( addr, randomData, defs::INCREMENTAL );
          std::copy(randomData.begin(), randomData.end(), registers.begin() + (addr - m_baseAddr));
          queuedTransactions.push_back( boost::shared_ptr<QueuedTransaction>( new QueuedBlockWrite( addr, blockSize, result) ) );
          nrQueuedWords += blockSize;

          break;
        }
      case 2:
        {// RMW-bits
          log ( Notice(), "Soak test - queueing: RMW-bits at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = rand();
          tempUInt2 = rand();
          vector<uint32_t>::iterator regIt = registers.begin() + (addr - m_baseAddr);

          if ( ipbus_vsn == 1 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          ValWord<uint32_t> result = client->rmw_bits ( addr, tempUInt1, tempUInt2 );
          queuedTransactions.push_back( boost::shared_ptr<QueuedTransaction>( new QueuedRmwBits( addr, tempUInt1, tempUInt2, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( ipbus_vsn == 2 )
          {
            *regIt &= tempUInt1;
            *regIt |= tempUInt2;
          }

          break;
        }
      case 3:
        {// RMW-sum
          log ( Notice(), "Soak test - queueing: RMW-sum at ", Integer ( addr, IntFmt<hex,fixed>() ) );
          tempUInt1 = rand();
          vector<uint32_t>::iterator regIt = registers.begin() + (addr - m_baseAddr);

          if ( ipbus_vsn == 1)
          {
            *regIt += tempUInt1;
          }

          ValWord<uint32_t> result = client->rmw_sum ( addr, tempUInt1 );
          queuedTransactions.push_back( boost::shared_ptr<QueuedTransaction>( new QueuedRmwSum( addr, tempUInt1, result, *regIt ) ) );
          nrQueuedWords += 1;

          if ( ipbus_vsn == 2 )
          {
            *regIt += tempUInt1;
          }

          break;
        }
    }
    
    if ( m_perIterationDispatch || (nrQueuedWords > 10000) || ( (i+1) == m_iterations ) )
    {
      log ( Notice(), "Soak test - issuing dispatch" );
      client->dispatch();

      log ( Notice(), "Soak test - issuing empty dispatch" );
      client->dispatch();

      for(vector< boost::shared_ptr<QueuedTransaction> >::const_iterator it = queuedTransactions.begin(); it != queuedTransactions.end(); it++)
      {
        if ( ! (*it)->check_values( m_verbose ) )
        {
          cout << "ERROR OCCURED IN SOAK TEST - after " << i << " successful transactions" << endl;
          return;
        }
      }
      queuedTransactions.clear();
      nrQueuedWords = 0;
    }

  }
  cout << "Reached end of soak test successfully!" << endl;
}


void uhal::tests::PerfTester::sandbox()
{
  try
  {
    BOOST_FOREACH ( ClientPtr& iClient, m_clients )
    {
      // *** Your code here ***
      // For example:
      ValWord<uint32_t> result = iClient->read ( m_baseAddr );
      iClient->dispatch();
      cout << "Read: " << std::hex << result.value() << " from address: " << m_baseAddr << std::dec << endl;
    }
  }
  catch ( const std::exception& e )
  {
    cout << e.what() << endl;
  }
}

// END OF PerfTester MEMBER FUNCTIONS


// The main() func...
int main ( int argc, char* argv[] )
{
  uhal::tests::PerfTester perfTester;
  return perfTester.run ( argc, argv );
}
