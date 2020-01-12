
#include "uhal/tests/fixtures.hpp"


#include "uhal/tests/PCIeDummyHardware.hpp"
#include "uhal/tests/TCPDummyHardware.hpp"
#include "uhal/tests/UDPDummyHardware.hpp"


namespace uhal {
namespace tests {


std::string AbstractFixture::connectionFileURI = "";
size_t AbstractFixture::timeout = 1;
bool AbstractFixture::quickTest = false;

std::string AbstractFixture::getAddressFileURI()
{
  std::string lURI(connectionFileURI);
  lURI.replace(lURI.size() - 15, 11, "address");
  return lURI;
}

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
  ConnectionManager manager(connectionFileURI);
  HwInterface hw(manager.getDevice(deviceId));
  hw.setTimeoutPeriod(timeout);
  return hw;
}


const DeviceType MinimalFixture<IPBUS_2_0_PCIE>::deviceType = IPBUS_2_0_PCIE;




template <>
DummyHardwareFixture<IPBUS_1_3_UDP>::DummyHardwareFixture() :
  hwRunner(new UDPDummyHardware<1,3>(devicePort, 0, false))
{
}

template <>
DummyHardwareFixture<IPBUS_1_3_CONTROLHUB>::DummyHardwareFixture() :
  hwRunner(new UDPDummyHardware<1,3>(devicePort, 0, false))
{
}

template <>
DummyHardwareFixture<IPBUS_1_3_TCP>::DummyHardwareFixture() :
  hwRunner(new TCPDummyHardware<1,3>(devicePort, 0, false))
{
}

template <>
DummyHardwareFixture<IPBUS_2_0_UDP>::DummyHardwareFixture() :
  hwRunner(new UDPDummyHardware<2,0>(devicePort, 0, false))
{
}

template <>
DummyHardwareFixture<IPBUS_2_0_CONTROLHUB>::DummyHardwareFixture() :
  hwRunner(new UDPDummyHardware<2,0>(devicePort, 0, false))
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

template <>
DummyHardwareFixture<IPBUS_2_0_TCP>::DummyHardwareFixture() :
  hwRunner(new TCPDummyHardware<2,0>(devicePort, 0, false))
{
}

template <>
DummyHardwareFixture<IPBUS_2_0_PCIE>::DummyHardwareFixture() :
  hwRunner(new PCIeDummyHardware(clientToHardwareFile, hardwareToClientFile, 0, false))
{
}

} // end ns tests
} // end ns uhal
