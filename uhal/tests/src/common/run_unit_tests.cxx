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


/* 
 * File:   run_unit_tests.cxx
 * Author: Tom Williams
 * Date:   September 2017
 */



#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE uhalTests

// BOOST_TEST_NO_MAIN: Disable auto-generation of main function, in order to define our own, which parses some arguments
#define BOOST_TEST_NO_MAIN


#include <boost/program_options.hpp>
#include <boost/test/unit_test.hpp>

#include "uhal/tests/tools.hpp"
#include "uhal/log/log.hpp"


using namespace uhal::tests;


std::map<std::string, DeviceInfo> getDeviceMap()
{
  std::map<std::string, DeviceInfo> lResult;

  lResult.insert(std::make_pair(std::string("1.3-udp"), DeviceInfo(IPBUS_1_3_UDP, "50001", "dummy.udp")));
  lResult.insert(std::make_pair(std::string("1.3-tcp"), DeviceInfo(IPBUS_1_3_TCP, "50002", "dummy.tcp")));
  lResult.insert(std::make_pair(std::string("1.3-hub"), DeviceInfo(IPBUS_1_3_CONTROLHUB, "50001", "dummy.controlhub")));
  lResult.insert(std::make_pair(std::string("2.0-udp"), DeviceInfo(IPBUS_2_0_UDP, "60001", "dummy.udp2")));
  lResult.insert(std::make_pair(std::string("2.0-tcp"), DeviceInfo(IPBUS_2_0_TCP, "60002", "dummy.tcp2")));
  lResult.insert(std::make_pair(std::string("2.0-hub"), DeviceInfo(IPBUS_2_0_CONTROLHUB, "60001", "dummy.controlhub2")));
  lResult.insert(std::make_pair(std::string("2.0-pcie"), DeviceInfo(IPBUS_2_0_PCIE, "/tmp/uhal_pcie_client2device,/tmp/uhal_pcie_device2client", "dummy.pcie2")));

  return lResult;
}

std::string getAvailableDevices()
{
  std::map<std::string, DeviceInfo> lDeviceMap = getDeviceMap();
  std::string lPossibleDeviceTypes;

  for(std::map<std::string, DeviceInfo>::const_iterator lIt=lDeviceMap.begin(); lIt != lDeviceMap.end(); lIt++) {
    lPossibleDeviceTypes += ("'" + lIt->first + "'");
    if (lIt != --lDeviceMap.end())
      lPossibleDeviceTypes += ", ";
  }

  return lPossibleDeviceTypes;
}


int BOOST_TEST_CALL_DECL
main( int argc, char* argv[] )
{
  namespace po = boost::program_options;

  const std::map<std::string, DeviceInfo> lDeviceMap = getDeviceMap();
  std::string lDeviceType;

  po::options_description lDesc ( "Allowed options" );
  lDesc.add_options()
  ( "help,h", "produce help message" )
  ( "connection-file,c", po::value<std::string>(&TestFixture::sConnectionFile)->required(), "Connection file URI" )
  ( "device-type,d", po::value<std::string>(&lDeviceType)->required(), ("Device type. Possible values: " + getAvailableDevices()).c_str() )
  ( "verbose,v", "Verbose output" )
  ( "very-verbose,V", "Very verbose output" )
  ;
  po::variables_map vm;


  std::vector<std::string> lOptionsForBoostUTF;
  try
  {
    po::parsed_options lParsedOptions = po::command_line_parser(argc, argv).options(lDesc).allow_unregistered().run();
    po::store ( lParsedOptions, vm );
    po::notify ( vm );
    lOptionsForBoostUTF = po::collect_unrecognized(lParsedOptions.options, po::include_positional);
  }
  catch ( std::exception& e )
  {
    std::cerr << "ERROR : " << e.what() << std::endl << std::endl;
    std::cout << "Usage : " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << lDesc << std::endl;
    exit ( 1 );
  }

  if ( vm.count ( "help" ) )
  {
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << lDesc << std::endl;
    exit ( 0 );
  }

  if (lDeviceMap.find(lDeviceType) == lDeviceMap.end()) {
    std::cout << "ERROR : Specified device type, '" << lDeviceType << "', is invalid." << std::endl;
    std::cout << "        Possible values are: " << getAvailableDevices() << std::endl;
    exit (1);
  }
  else
    TestFixture::sDeviceInfo = lDeviceMap.find(lDeviceType)->second;


  std::cout << "Supplied arguments ..." << std::endl;
  std::cout << "   connection file = " << TestFixture::sConnectionFile << std::endl;
  std::cout << "   device type     = " << lDeviceType << std::endl;
  TestFixture::sDeviceId = TestFixture::sDeviceInfo.connectionId;

  std::cout << "Log level set to ";
  if ( vm.count ( "very-verbose" ) ) {
    uhal::setLogLevelTo ( uhal::Debug() );
    std::cout << "DEBUG";
  }
  else if ( vm.count ( "verbose" ) ) {
    uhal::setLogLevelTo ( uhal::Notice() );
    std::cout << "NOTICE";
  }
  else {
    uhal::setLogLevelTo ( uhal::Fatal() );
    std::cout << "FATAL";
  }
  std::cout << std::endl << std::endl;


  std::vector<const char*> lArgvForBoostUTF;
  lArgvForBoostUTF.push_back(argv[0]);
  if (lOptionsForBoostUTF.empty())
    std::cout << "N.B. Didn't find any arguments/options to pass to boost UTF" << std::endl;
  else {
    std::cout << "Passing " << lOptionsForBoostUTF.size() << " arguments/options to boost UTF:" << std::endl << "  ";
    for (size_t i=0; i<lOptionsForBoostUTF.size(); i++) {
      std::cout << " " << lOptionsForBoostUTF.at(i);
      lArgvForBoostUTF.push_back(lOptionsForBoostUTF.at(i).c_str());
    }
  }
  std::cout << std::endl << std::endl;

  lArgvForBoostUTF.push_back(0);
  return ::boost::unit_test::unit_test_main( &init_unit_test, lArgvForBoostUTF.size()-1, const_cast<char**>(lArgvForBoostUTF.data()) );
}
