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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/ClientFactory.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolIPbus.hpp"
#include "uhal/ProtocolControlHub.hpp"
#include "uhal/ProtocolMmap.hpp"
#include "uhal/ProtocolPCIe.hpp"
#include "uhal/tests/DummyClient.hpp"

#include <boost/test/unit_test.hpp>


namespace uhal {
namespace tests {


template <class T>
void checkClientFactory(const std::string& aId, const std::string& aURI, const std::vector<std::string>& aUserClientActivationList)
{
  BOOST_TEST_MESSAGE("Creating client with ID '" << aId << "', URI '" << aURI << "' (" << aUserClientActivationList.size() << " user clients activated)");
  for(size_t i=0; i < aUserClientActivationList.size(); i++) {
    BOOST_TEST_MESSAGE("  - '" << aUserClientActivationList.at(i) << "'");
  }

  std::shared_ptr<ClientInterface> lClient(ClientFactory::getInstance().getClient(aId, aURI, aUserClientActivationList));
  BOOST_REQUIRE(lClient);
  BOOST_CHECK_EQUAL(lClient->id(), aId);
  BOOST_CHECK_EQUAL(lClient->uri(), aURI);
  BOOST_CHECK(typeid(*lClient) == typeid(T));
}


template <class T>
void checkClientFactory(const std::string& aId, const std::string& aURI)
{
  BOOST_TEST_MESSAGE("Creating client with ID '" << aId << "', URI '" << aURI << "'");
  std::shared_ptr<ClientInterface> lClient(ClientFactory::getInstance().getClient(aId, aURI));
  BOOST_REQUIRE(lClient);
  BOOST_CHECK_EQUAL(lClient->id(), aId);
  BOOST_CHECK_EQUAL(lClient->uri(), aURI);
  BOOST_CHECK(typeid(*lClient) == typeid(T));

  checkClientFactory<T>(aId, aURI, std::vector<std::string>());
}


BOOST_AUTO_TEST_SUITE( client_factory )


BOOST_AUTO_TEST_CASE (core_clients)
{
  checkClientFactory<UDP<IPbus<1,3> > >("alice", "ipbusudp-1.3://localhost:50001");
  checkClientFactory<UDP<IPbus<2,0> > >("bob", "ipbusudp-2.0://localhost:50001");
  checkClientFactory<TCP<IPbus<1,3>, 1 > >("charlie", "ipbustcp-1.3://localhost:50001");
  checkClientFactory<TCP<IPbus<2,0>, 1 > >("dave", "ipbustcp-2.0://localhost:50001");
  checkClientFactory<TCP<ControlHub < IPbus< 1 , 3 > > , 3 > >("x", "chtcp-1.3://localhost:12345?target=localhost:50001");
  checkClientFactory<TCP<ControlHub < IPbus< 2 , 0 > > , 3 > >("y", "chtcp-2.0://localhost:12345?target=localhost:50001");
  checkClientFactory<PCIe>("z", "ipbuspcie-2.0:///path1,/path2");
  checkClientFactory<Mmap>("z", "ipbusmmap-2.0:///path/to/file");

  BOOST_CHECK_THROW(ClientFactory::getInstance().getClient("some_id", "unknown_protocol://localhost:50001"), exception::ProtocolDoesNotExist);
}


BOOST_AUTO_TEST_CASE (user_clients)
{
  checkClientFactory<DummyClient>("bob", "__test__://localhost:50001", std::vector<std::string>(1, "__test__"));

  BOOST_CHECK_THROW(ClientFactory::getInstance().getClient("bob", "__test__://localhost:50001"), exception::ProtocolNotEnabled);
  BOOST_CHECK_THROW(ClientFactory::getInstance().getClient("bob", "__test__://localhost:50001", std::vector<std::string>()), exception::ProtocolNotEnabled);
}


BOOST_AUTO_TEST_SUITE_END()

} // end ns tests
} // end ns uhal
