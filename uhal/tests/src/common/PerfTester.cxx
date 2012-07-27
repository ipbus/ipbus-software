#include "uhal/tests/PerfTester.hpp"

// C++ headers
#include <iostream>
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

uhal::tests::PerfTester::PerfTester():
  m_testDescMap(),
  m_testFuncMap(),
  m_deviceURIs(),
  m_clients(),
  m_testName("BandwidthRx"),
  m_iterations(1000),
  m_baseAddrStr("0x0"),
  m_baseAddr(0),
  m_bandwidthTestDepth(0),
  m_verbose(false),
  m_perIterationDispatch(false)
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
  srand(time(NULL));
}


int uhal::tests::PerfTester::run(int argc, char* argv[])
{
  try
  {
    // This just defines and parses the command line parameters that are allowed.
    po::options_description argDescriptions("Allowed options");
    argDescriptions.add_options()
        ("help,h", "Produce help message")
        ("list,l", "List the names of the different tests that can be performed.")
        ("verbose,v", "Make the output more verbose.")
        ("test,t", po::value<string>(&m_testName)->default_value("BandwidthRx"), "Name of the test to be performed.")
        ("iterations,i", po::value<unsigned>(&m_iterations)->default_value(1000), "Number of test iterations to run.")
        ("devices,d", po::value<StringVec>(&m_deviceURIs)->multitoken(), "List of device connection URIs, e.g. chtcp-1.3://..., etc")
        ("baseAddr,b", po::value<string>(&m_baseAddrStr)->default_value("0x0"), "Base address (in hex) of the test location on the target device(s).")
        ("bandwidthTestDepth,w", po::value<boost::uint32_t>(&m_bandwidthTestDepth)->default_value(340), "Depth of read/write used in bandwidth tests.")
        ("perIterationDispatch,p", "Force a network dispatch every test iteration instead of the default single dispatch call at the end.");

    po::variables_map argMap;
    po::store(po::parse_command_line(argc, argv, argDescriptions), argMap);
    po::notify(argMap);

    // Convert the hexadecimal base addr value into an actual number.
    // This is just a workaround for boost:program_options not understanding hex input
    istringstream convert(m_baseAddrStr);  convert >> std::hex >> m_baseAddr;
    if(convert.fail()) { cerr << "The specified base address was not valid hexadecimal!" << endl; return 10; }

    // Display help info and exit
    if(argc < 2 || argMap.count("help")) { ostringstream oss; oss << argDescriptions; outputHelpText(oss.str()); return 20; }
    if(argMap.count("list")) { outputTestDescriptionsList(); return 30; }
    if(argMap.count("verbose")) { m_verbose = true; }
    if(argMap.count("perIterationDispatch")) { m_perIterationDispatch = true; }

    if(badInput()) { return 40; }  // Report bad user input and exit if necessary.

    outputUserChoices();  // Echo back to the user the settings they have selected.

    buildClients(); // Build the clients from the device URIs provided by the user

    (this->*m_testFuncMap.find(m_testName)->second)();  // Calls the test function, based on the test name.
  }
  catch(std::exception& e) { cerr << "Error:  " << e.what() << endl; return 50; }
  catch(...) { cerr << "Caught exception of unknown type!" << endl; return 60; }

  return 0;  // The program ran successfully (even if the test didn't...)
}


// PRIVATE METHODS - Test infrastructure

void uhal::tests::PerfTester::outputHelpText(const string& argDescriptions) const
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
  cout << "  " << setw(16) << left << "Name" << "  " << "Description" << endl;
  cout << "  " << setw(16) << left << "----" << "  " << "-----------" << endl;
  TestDescMap::const_iterator iTest = m_testDescMap.begin(), iTestEnd = m_testDescMap.end();
  for( ; iTest != iTestEnd ; ++iTest)
  {
    cout << "  " << setw(16) << left << iTest->first << "  "  << iTest->second << endl;
  }
  cout << endl;
}


bool uhal::tests::PerfTester::badInput() const
{
  if(m_deviceURIs.empty())
  {
    cerr << "You must specify at least one device connection URI by using the -d option!" << endl; return true;
  }

  if(m_testFuncMap.find(m_testName) == m_testFuncMap.end())
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
       << "  Per-iteration dispatch -->  " << (m_perIterationDispatch?"Yes":"No") << endl
       << "  Device URIs:" << endl;
  StringVec::const_iterator iDevice = m_deviceURIs.begin(), iDeviceEnd = m_deviceURIs.end();
  for(; iDevice != iDeviceEnd ; ++iDevice) { cout << "    " << *iDevice << endl; }
  cout << "\nRunning test now...\n" << endl;
}


void uhal::tests::PerfTester::buildClients()
{
  if(m_verbose) { setLogLevelTo( Info() ); cout << "Building device clients..." << endl; }
  else { setLogLevelTo( Warning() ); }

  m_clients.reserve(m_deviceURIs.size());
  for(unsigned int iURI = 0 ; iURI < m_deviceURIs.size() ; ++iURI)
  {
    m_clients.push_back(ClientFactory::getInstance().getClient("MyDevice", m_deviceURIs.at(iURI)));
  }

  if(m_verbose) { cout << "Device clients built successfully!" << endl; }
}


void uhal::tests::PerfTester::outputStandardResults(double totalSeconds) const
{
  string underline; underline.assign(m_testName.size() + 14, '-');
  cout << m_testName << " Test Results:\n"
       << underline << "\n\n"
       << "Number of IPbus hosts in test   = " << m_deviceURIs.size() << "\n"
       << "Total test iterations           = " << m_iterations << "\n"
       << "Total time taken                = " << totalSeconds << " s\n"
       << "Test iteration frequency        = " << m_iterations/totalSeconds << " Hz" << endl;
}


uhal::tests::PerfTester::U32Vec uhal::tests::PerfTester::getRandomBuffer(unsigned size) const
{
  U32Vec buffer; buffer.reserve(size);
  // Never said anything about it being a flat random distribution... ;-)
  for(unsigned i = 0 ; i < size ; ++i) { buffer.push_back(rand() + rand()); }
  return buffer;
}


bool uhal::tests::PerfTester::buffersEqual(const U32Vec& writeBuffer, const U32ValVec& readBuffer) const
{
  return std::equal(readBuffer.begin(), readBuffer.end(), writeBuffer.begin());
}


// PRIVATE MEMBER FUNCTIONS - IPbus test functions that users can run

void uhal::tests::PerfTester::bandwidthRxTest()
{
  Timer myTimer;
  for(unsigned i = 0; i < m_iterations ; ++i)
  {
    if(m_verbose) { cout << "Iteration " << i << endl; }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->readBlock(m_baseAddr, m_bandwidthTestDepth, defs::NON_INCREMENTAL); }
    if(m_perIterationDispatch) { BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); } }
  }
  if(!m_perIterationDispatch) { BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); } }

  double totalSeconds = myTimer.elapsedSeconds();
  double totalPayloadKB = m_deviceURIs.size() * m_iterations * m_bandwidthTestDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;

  outputStandardResults(totalSeconds);
  cout << "Read depth used each iteration  = " << m_bandwidthTestDepth << " 32-bit words\n"
       << "Total IPbus payload received    = " << totalPayloadKB << " KB\n"
       << "Average read bandwidth          = " << dataRateKB_s << " KB/s" << endl;
}


void uhal::tests::PerfTester::bandwidthTxTest()
{
  // Send buffer - lots of "cafebabe" (in little-endian)
  U32Vec sendBuffer(m_bandwidthTestDepth, 0xbebafeca);

  Timer myTimer;
  for(unsigned i = 0; i < m_iterations ; ++i)
  {
    if(m_verbose) { cout << "Iteration " << i << endl; }
    // Create the packet
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->writeBlock(m_baseAddr, sendBuffer, defs::NON_INCREMENTAL); }
    if(m_perIterationDispatch) { BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); } }
  }
  if(!m_perIterationDispatch) { BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); } }

  double totalSeconds = myTimer.elapsedSeconds();
  double totalPayloadKB = m_deviceURIs.size() * m_iterations * m_bandwidthTestDepth * 4. / 1024.;
  double dataRateKB_s = totalPayloadKB/totalSeconds;

  outputStandardResults(totalSeconds);
  cout << "Write depth used each iteration = " << m_bandwidthTestDepth << " 32-bit words\n"
       << "Total IPbus payload sent        = " << totalPayloadKB << " KB\n"
       << "Average write bandwidth         = " << dataRateKB_s << " KB/s" << endl;
}


void uhal::tests::PerfTester::validationTest()
{
  // Historic basic firmware/software validation test

  uint32_t tempWord;  // A single word buffer for holding temporary test data
  U32Vec tempBuffer;  // A buffer for holding temporary test data

  unsigned nFail = 0;
  string tmpStr;
  ostringstream summary;
  unsigned iDepth = 1; // For variable depth transaction tests.
  const unsigned maxTestDepth = 380;  // Needs to be bigger than the max size that will fit into a standard frame UDP packet (i.e >368)

  cout << "\n1) No-throw test: Single unmasked register read:" << endl;
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->read(m_baseAddr); }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n2) No-throw test: Single unmasked register write:" << endl;
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->write(m_baseAddr, 0xdeadbeef); }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n3) No-throw test: Single masked register read:" << endl;
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->read(m_baseAddr, 0xf000000f); }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n4) No-throw test: Single masked register write:" << endl;
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->write(m_baseAddr, 0x90000009, 0xf000000f); }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n5) No-throw test: Read transaction, depths 1 - " << maxTestDepth << ":" << endl;
  try
  {
    for(iDepth = 1 ; iDepth <= maxTestDepth ; ++iDepth)
    {
      if(m_verbose) { cout << "Performing read transaction, depth = " << iDepth << endl; }
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->readBlock(m_baseAddr, iDepth); }
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL at depth = " << iDepth << "!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n6) No-throw test: Non-incr. read transaction, depths 1 - " << maxTestDepth << ":" << endl;
  try
  {
    for(iDepth = 1 ; iDepth <= maxTestDepth ; ++iDepth)
    {
      if(m_verbose) { cout << "Performing non-incr. read transaction, depth = " << iDepth << endl; }
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->readBlock(m_baseAddr, iDepth, defs::NON_INCREMENTAL); }
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL at depth = " << iDepth << "!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n7) No-throw test: Write transaction, depths 1 - " << maxTestDepth << ":" << endl;
  try
  {
    for(iDepth = 1 ; iDepth <= maxTestDepth ; ++iDepth)
    {
      if(m_verbose) { cout << "Performing write transaction, depth = " << iDepth << endl; }
      U32Vec dataToWrite(iDepth, 0xdeadcafe);
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->writeBlock(m_baseAddr, dataToWrite); }
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL at depth = " << iDepth << "!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n8) No-throw test: Non-incr. write transaction, depths 1 - " << maxTestDepth << ":" << endl;
  try
  {
    for(iDepth = 1 ; iDepth <= maxTestDepth ; ++iDepth)
    {
      if(m_verbose) { cout << "Performing non-incr. write transaction, depth = " << iDepth << endl; }
      U32Vec dataToWrite(iDepth, 0xdeafbabe);
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->writeBlock(m_baseAddr, dataToWrite, defs::NON_INCREMENTAL); }
      BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL at depth = " << iDepth << "!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n9) No-throw test: RMW-bits transaction:" << endl;
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->rmw_bits(m_baseAddr, 0x000fffff, 0x9ba11); }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n10) No-throw test: RMW-sum transaction:" << endl;
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->rmw_sum(m_baseAddr, 1); }
    BOOST_FOREACH(ClientPtr& iClient, m_clients) { iClient->dispatch(); }
    cout << "\tOK" << endl;
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n11) Write read-back test: Random value to single register:" << endl;
  tempWord = getRandomBuffer(1).at(0);
  try
  {
    bool testFail = false;
    BOOST_FOREACH(ClientPtr& iClient, m_clients)
    {
      iClient->write(m_baseAddr, tempWord);
      ValWord<uint32_t> result = iClient->read(m_baseAddr);
      iClient->dispatch();
      if(tempWord != result.value()) { testFail = true; }
    }
    if(!testFail) { cout << "\tOK" << endl; }
    else { cout << "\tFAIL!  <----- PROBLEM!" << endl; ++nFail; }
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n12) Write read-back test: Random values, depth = 500:" << endl;
  tempBuffer = getRandomBuffer(500);
  try
  {
    bool testFail = false;
    BOOST_FOREACH(ClientPtr& iClient, m_clients)
    {
      iClient->writeBlock(m_baseAddr, tempBuffer);
      U32ValVec result = iClient->readBlock(m_baseAddr, 500);
      iClient->dispatch();
      if(!buffersEqual(tempBuffer, result)) { testFail = true; }
    }
    if(!testFail) { cout << "\tOK" << endl; }
    else { cout << "\tFAIL!  <----- PROBLEM!" << endl; ++nFail; }
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n13) RMW-bits test:" << endl;
  try
  {
    bool testFail = false;
    BOOST_FOREACH(ClientPtr& iClient, m_clients)
    {
      iClient->write(m_baseAddr, 0xf6e5d4c3); // Write a start value of 0xf6e5d4c3 into the test register
      ValWord<uint32_t> result1 = iClient->rmw_bits(m_baseAddr, 0xffff0000, 0x5e6f);
      ValWord<uint32_t> result2 = iClient->rmw_bits(m_baseAddr, 0x0000ffff, 0x3c4d0000);
      ValWord<uint32_t> result3 = iClient->read(m_baseAddr);  // Read back the result normally to double-check.
      iClient->dispatch();
      if(result1.value() != 0xf6e55e6f) { testFail = true; }
      if(result2.value() != 0x3c4d5e6f) { testFail = true; }
      if(result3.value() != 0x3c4d5e6f) { testFail = true; }
    }
    if(testFail) { cout << "\tFAIL!  <----- PROBLEM!" << endl; ++nFail; }
    else { cout << "\tOK" << endl; }
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n14) RMW-sum test:" << endl;
  try
  {
    bool testFail = false;
    BOOST_FOREACH(ClientPtr& iClient, m_clients)
    {
      iClient->write(m_baseAddr, 0x00000000); // Write a start value of 0x00000000 into the test register
      ValWord<uint32_t> result1 = iClient->rmw_sum(m_baseAddr, 0xabcd);  // Add 0xabcd to 0...
      ValWord<uint32_t> result2 = iClient->rmw_sum(m_baseAddr, 0xffff5434);  // Add 0xffff5434 to 0xabcd... this should overflow past the 32-bit max value and result in a 1.
      ValWord<uint32_t> result3 = iClient->read(m_baseAddr);  // Read back the result normally to double-check.
      iClient->dispatch();
      if(result1 != 0x0000abcd) { testFail = true; }
      if(result2 != 0x00000001) { testFail = true; }
      if(result3 != 0x00000001) { testFail = true; }
    }
    if(testFail) { cout << "\tFAIL!  <----- PROBLEM!" << endl; ++nFail; }
    else { cout << "\tOK" << endl; }
  }
  catch(const std::exception& e) { cout << "\tFAIL!  <----- PROBLEM!\n" << e.what() << endl; ++nFail; }

  cout << "\n----\nValidation test summary: " << nFail << " test(s) failed." << endl;
}


void uhal::tests::PerfTester::sandbox()
{
  try
  {
    BOOST_FOREACH(ClientPtr& iClient, m_clients)
    {
      // *** Your code here ***
      // For example:
      ValWord<uint32_t> result = iClient->read(m_baseAddr);
      iClient->dispatch();
      cout << "Read: " << std::hex << result.value() << " from address: " << m_baseAddr << std::dec << endl;
    }
  }
  catch(const std::exception& e) { cout << e.what() << endl; }
}

// END OF PerfTester MEMBER FUNCTIONS


// The main() func...
int main(int argc, char* argv[]) { uhal::tests::PerfTester perfTester; return perfTester.run(argc, argv); }
