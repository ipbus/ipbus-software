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
#include <boost/mem_fn.hpp>

// uHAL headers
#include "uhal/ClientFactory.hpp"
#include "uhal/tests/tools.hpp"

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
  m_perIterationDispatch ( false ),
  m_includeConnect ( false )
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
    ( "iterations,i", po::value<uint64_t> ( &m_iterations )->default_value ( 1000 ), "Number of test iterations to run." )
    ( "devices,d", po::value<StringVec> ( &m_deviceURIs )->multitoken(), "List of device connection URIs, e.g. chtcp-1.3://..., etc" )
    ( "baseAddr,b", po::value<string> ( &m_baseAddrStr )->default_value ( "0x0" ), "Base address (in hex) of the test location on the target device(s)." )
    ( "bandwidthTestDepth,w", po::value<boost::uint32_t> ( &m_bandwidthTestDepth )->default_value ( 340 ), "Depth of read/write used in bandwidth tests." )
    ( "perIterationDispatch,p", "Force a network dispatch every test iteration instead of the default single dispatch call at the end." )
    ( "includeConnect,c", "Include connect time in reported bandwidths and latencies" );
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

    if ( argMap.count ( "includeConnect" ) )
    {
      m_includeConnect = true;
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
    cerr << "Error - exception thrown ..." << endl << e.what() << endl;
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


bool uhal::tests::PerfTester::buffersEqual ( const U32Vec& writeBuffer, const U32ValVec& readBuffer ) const
{
  return std::equal ( readBuffer.begin(), readBuffer.end(), writeBuffer.begin() );
}


// PRIVATE MEMBER FUNCTIONS - IPbus test functions that users can run

void uhal::tests::PerfTester::bandwidthRxTest()
{
  if ( ! m_includeConnect )
  {
    for ( ClientPtr& iClient: m_clients )
    {
      iClient->readBlock ( m_baseAddr, 1, defs::NON_INCREMENTAL );
      iClient->dispatch();
    }
  }

  std::vector<ClientInterface*> lClients;
  for ( ClientPtr& iClient: m_clients )
  {
    lClients.push_back( &*iClient );
  }

  double totalSeconds = measureReadLatency(lClients, m_baseAddr, m_bandwidthTestDepth, m_iterations, m_perIterationDispatch, m_verbose);
  double totalPayloadKB = m_deviceURIs.size() * m_iterations * m_bandwidthTestDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;
  outputStandardResults ( totalSeconds );
  cout << "Read depth used each iteration  = " << m_bandwidthTestDepth << " 32-bit words\n"
       << "Total IPbus payload received    = " << totalPayloadKB << " KB\n"
       << "Average read bandwidth          = " << dataRateKB_s << " KB/s" << endl;
}


void uhal::tests::PerfTester::bandwidthTxTest()
{
  if ( ! m_includeConnect )
  {
    for ( ClientPtr& iClient: m_clients )
    {
      iClient->writeBlock ( m_baseAddr, std::vector<uint32_t>(1,0x0), defs::NON_INCREMENTAL );
      iClient->dispatch();
    }
  }

  std::vector<ClientInterface*> lClients;
  for ( ClientPtr& iClient: m_clients )
  {
    lClients.push_back( &*iClient );
  }

  double totalSeconds = measureWriteLatency(lClients, m_baseAddr, m_bandwidthTestDepth, m_iterations, m_perIterationDispatch, m_verbose);
  double totalPayloadKB = m_deviceURIs.size() * m_iterations * m_bandwidthTestDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;
  outputStandardResults ( totalSeconds );
  cout << "Write depth used each iteration = " << m_bandwidthTestDepth << " 32-bit words\n"
       << "Total IPbus payload sent        = " << totalPayloadKB << " KB\n"
       << "Average write bandwidth         = " << dataRateKB_s << " KB/s" << endl;
}


void uhal::tests::PerfTester::validationTest()
{
  std::vector<ClientInterface*> lClients;
  for (const auto& c: m_clients)
    lClients.push_back(&*c);

  if (not runValidationTest(lClients, m_baseAddr, m_bandwidthTestDepth, m_iterations, m_perIterationDispatch, m_verbose) )
    std::exit( 1 );
}


void uhal::tests::PerfTester::sandbox()
{
  try
  {
    for ( ClientPtr& iClient: m_clients )
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
