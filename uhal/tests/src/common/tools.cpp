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


#include <boost/program_options.hpp>

#include "uhal/ConnectionManager.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/log/log.hpp"
#include "uhal/tests/TCPDummyHardware.hpp"
#include "uhal/tests/UDPDummyHardware.hpp"


namespace po = boost::program_options;


namespace uhal {
namespace tests {


std::string AbstractFixture::sConnectionFile = "";

template <>
MinimalFixture<IPBUS_1_3_UDP>::MinimalFixture() : 
  devicePort(50001),
  deviceId("dummy.udp")
{
}

template <>
MinimalFixture<IPBUS_1_3_TCP>::MinimalFixture() :
  devicePort(50002),
  deviceId("dummy.tcp")
{
}

template <>
MinimalFixture<IPBUS_1_3_CONTROLHUB>::MinimalFixture() :
  devicePort(50001),
  deviceId("dummy.controlhub")
{
}

template <>
MinimalFixture<IPBUS_2_0_UDP>::MinimalFixture() :
  devicePort(60001),
  deviceId("dummy.udp2")
{
}

template <>
MinimalFixture<IPBUS_2_0_TCP>::MinimalFixture() :
  devicePort(60002),
  deviceId("dummy.tcp2")
{
}

template <>
MinimalFixture<IPBUS_2_0_CONTROLHUB>::MinimalFixture() :
  devicePort(60001),
  deviceId("dummy.controlhub2")
{
}

MinimalFixture<IPBUS_2_0_PCIE>::MinimalFixture() :
  hardwareToClientFile("/tmp/uhal_pcie_device2client"),
  clientToHardwareFile("/tmp/uhal_pcie_client2device"),
  deviceId("dummy.pcie2")
{
}


MinimalFixture<IPBUS_2_0_PCIE>::~MinimalFixture()
{
}


HwInterface MinimalFixture<IPBUS_2_0_PCIE>::getHwInterface() const
{
  ConnectionManager manager(sConnectionFile);
  return manager.getDevice(deviceId);
}


const DeviceType MinimalFixture<IPBUS_2_0_PCIE>::deviceType = IPBUS_2_0_PCIE;


template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_1_3_UDP>::createRunner (const uint16_t aPort)
{
  return new DummyHardwareRunner<UDPDummyHardware<1,3> >(aPort, 0, false);
}

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_1_3_CONTROLHUB>::createRunner (const uint16_t aPort)
{
  return new DummyHardwareRunner<UDPDummyHardware<1,3> >(aPort, 0, false);
}

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_1_3_TCP>::createRunner (const uint16_t aPort)
{
  return new DummyHardwareRunner<TCPDummyHardware<1,3> >(aPort, 0, false);
}

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_2_0_UDP>::createRunner (const uint16_t aPort)
{
  return new DummyHardwareRunner<UDPDummyHardware<2,0> >(aPort, 0, false);
}

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_2_0_CONTROLHUB>::createRunner (const uint16_t aPort)
{
  return new DummyHardwareRunner<UDPDummyHardware<2,0> >(aPort, 0, false);
}

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_2_0_TCP>::createRunner (const uint16_t aPort)
{
  return new DummyHardwareRunner<TCPDummyHardware<2,0> >(aPort, 0, false);
}


template <>
DummyHardwareFixture<IPBUS_2_0_CONTROLHUB>::DummyHardwareFixture() :
  hwRunner(createRunner(devicePort))
{
  // FIXME : Ensure that controlhub cache is reset after dummy hardware reboot, but before unit tests (temporary solution)
  if ( deviceType == IPBUS_2_0_CONTROLHUB ) {
    HwInterface hw = getHwInterface();
    hw.getClient().read(0);
    try {
      hw.dispatch();
    }
    catch ( ... ) {
    }
  }
}


DummyHardwareFixture<IPBUS_2_0_PCIE>::DummyHardwareFixture() :
  hwRunner(new DummyHardwareRunner<PCIeDummyHardware>(clientToHardwareFile, hardwareToClientFile, 0, false))
{
}

DummyHardwareFixture<IPBUS_2_0_PCIE>::~DummyHardwareFixture() 
{
}


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
