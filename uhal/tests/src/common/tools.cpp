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

#include "uhal/tests/tools.hpp"


#include "uhal/ConnectionManager.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/log/log.hpp"
#include "uhal/tests/UDPDummyHardware.hpp"
#include "uhal/tests/TCPDummyHardware.hpp"

#include <boost/program_options.hpp>


namespace po = boost::program_options;


namespace uhal {
namespace tests {


DeviceInfo::DeviceInfo(uhal::tests::DeviceType aType, const std::string& aPort, const std::string& aConnectionId) : 
  type(aType),
  port(aPort),
  connectionId(aConnectionId)
{
}


template<>
DummyHardwareRunner<PCIeDummyHardware>::DummyHardwareRunner(const std::string& aPort , const uint32_t& aReplyDelay, const bool& aBigEndianHack) : 
  mHw(aPort.substr(0, aPort.find(",")), aPort.substr(aPort.find(",")+1), aReplyDelay, aBigEndianHack),
  mHwThread( boost::bind(&PCIeDummyHardware::run, &mHw))
{
}


TestFixture::TestFixture() :
  hwRunner(createRunner(sDeviceInfo))
{
  // FIXME : Ensure that controlhub cache is reset after dummy hardware reboot, but before unit tests (temporary solution)
  if ( sDeviceInfo.type == IPBUS_2_0_CONTROLHUB ) {
    ConnectionManager manager ( sConnectionFile );
    HwInterface hw=manager.getDevice ( sDeviceId );
    hw.getClient().read(0);
    try {
      hw.dispatch();
    }
    catch ( ... ) {
    }
  }
}


TestFixture::~TestFixture()
{
}


boost::shared_ptr<DummyHardwareRunnerInterface> TestFixture::createRunner (const DeviceInfo& aDetails)
{
  boost::shared_ptr<DummyHardwareRunnerInterface> lResult;

  switch (aDetails.type) {
    case IPBUS_1_3_UDP :
    case IPBUS_1_3_CONTROLHUB : 
      lResult.reset(new DummyHardwareRunner<UDPDummyHardware<1,3> >(aDetails.port, 0, false));
      break;
    case IPBUS_1_3_TCP :
      lResult.reset(new DummyHardwareRunner<TCPDummyHardware<1,3> >(aDetails.port, 0, false));
      break;
    case IPBUS_2_0_UDP : 
    case IPBUS_2_0_CONTROLHUB : 
      lResult.reset(new DummyHardwareRunner<UDPDummyHardware<2,0> >(aDetails.port, 0, false));
      break;
    case IPBUS_2_0_TCP :
      lResult.reset(new DummyHardwareRunner<TCPDummyHardware<2,0> >(aDetails.port, 0, false));
      break;
    case IPBUS_2_0_PCIE : 
      lResult.reset(new DummyHardwareRunner<PCIeDummyHardware>(aDetails.port, 0, false));
      break;
  }

  return lResult;
}


std::string TestFixture::sConnectionFile = "";
DeviceInfo TestFixture::sDeviceInfo(IPBUS_2_0_UDP, "60001", "dummy.udp2");
std::string TestFixture::sDeviceId = "";


double measureRxPerformance(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, std::ostream* aOutStream)
{
  typedef std::vector<ClientInterface*>::const_iterator ClientIterator_t;
  Timer myTimer;

  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aOutStream )
    {
      (*aOutStream) << "Iteration " << i << std::endl;
    }

    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->readBlock ( aBaseAddr, aDepth, defs::NON_INCREMENTAL );
    }

    if ( aDispatchEachIteration )
    {
      for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
      {
        (*lIt)->dispatch();
      }
    }
  }

  if ( !aDispatchEachIteration )
  {
    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->dispatch();
    }
  }

  return myTimer.elapsedSeconds();
}


double measureTxPerformance(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, std::ostream* aOutStream)
{
  typedef std::vector<ClientInterface*>::const_iterator ClientIterator_t;

  // Send buffer - lots of "cafebabe" (in little-endian)
  std::vector<uint32_t> sendBuffer ( aDepth, 0xbebafeca );
  Timer myTimer;

  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aOutStream )
    {
      (*aOutStream) << "Iteration " << i << std::endl;
    }

    // Create the packet
    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->writeBlock ( aBaseAddr, sendBuffer, defs::NON_INCREMENTAL );
    }

    if ( aDispatchEachIteration )
    {
      for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
      {
        (*lIt)->dispatch();
      }
    }
  }

  if ( !aDispatchEachIteration )
  {
    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->dispatch();
    }
  }

  return myTimer.elapsedSeconds();
}


} // end ns tests
} // end ns uhal


uint32_t uhal::tests::failedTestCount = 0;
uint32_t uhal::tests::passedTestCount = 0;


long uhal::tests::usdiff ( const timeval& end, const timeval& start )
{
  long usec ( 0 );
  usec= ( end.tv_sec-start.tv_sec ) *1000000;
  usec+= ( end.tv_usec-start.tv_usec );
  return usec;
}

std::map<std::string,std::string> uhal::tests::default_arg_parsing ( int argc,char* argv[] )
{
  // Declare the supported options.
  po::options_description desc ( "Allowed options" );
  desc.add_options()
  ( "help,h", "produce help message" )
  ( "connection_file,c", po::value<std::string>()->default_value ( "", "Connection file URI" ) )
  ( "device_id,d", po::value<std::string>()->default_value ( "", "Device identifier" ) )
  ( "verbose,v", "Verbose output" )
  ( "very_verbose,V", "Very verbose output" )
  ;
  po::variables_map vm;

  try
  {
    po::store ( po::parse_command_line ( argc, argv, desc ), vm );
    po::notify ( vm );
  }
  catch ( std::exception& e )
  {
    std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << desc << std::endl;
    exit ( 1 );
  }

  if ( vm.count ( "help" ) )
  {
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << desc << std::endl;
    exit ( 0 );
  }

  std::map<std::string,std::string> result;
  result["connection_file"] = vm["connection_file"].as<std::string>();
  result["device_id"] = vm["device_id"].as<std::string>();

  if ( vm.count ( "very_verbose" ) )
  {
    uhal::setLogLevelTo ( Debug() );
    result["very_verbose"] = "true";
    result["verbose"] = "true";
  }
  else if ( vm.count ( "verbose" ) )
  {
    uhal::setLogLevelTo ( Notice() );
    result["very_verbose"] = "false";
    result["verbose"] = "true";
  }
  else
  {
    uhal::setLogLevelTo ( Fatal() );
    result["very_verbose"] = "false";
    result["verbose"] = "false";
  }

  return result;
}

