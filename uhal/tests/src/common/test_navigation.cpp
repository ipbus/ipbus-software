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

#include "uhal/uhal.hpp"

#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/test/unit_test.hpp>

#include <vector>
#include <string>
#include <iostream>
#include <cstdlib>
#include <typeinfo>
#include <iterator>


namespace uhal {
namespace tests {


void iteration ( const uhal::Node& parentNode )
{ 
  uint32_t lAddr = 0x0;

  for(uhal::Node::const_iterator lIt = parentNode.begin(); lIt != parentNode.end(); lIt++)
  {
    BOOST_CHECK ( lIt->getAddress() >= lAddr );
    lAddr = lIt->getAddress();
  }
}

UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(NodeNavigationTestSuite, navigation_and_traversal, MinimalFixture,
{
  HwInterface hw = getHwInterface();

  std::vector<std::string> ids = hw.getNodes();
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.MEM" ) != ids.end() );
  ids = hw.getNodes ( ".*MEM.*" );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"LARGE_MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SMALL_MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.MEM" ) != ids.end() );
  ids = hw.getNode ( "SUBSYSTEM1" ).getNodes();
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"MEM" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBMODULE.REG" ) != ids.end() );
  BOOST_CHECK ( std::find ( ids.begin(),ids.end(),"SUBMODULE.MEM" ) != ids.end() );

  BOOST_CHECK_NO_THROW ( iteration ( hw.getNode() ) );
  BOOST_CHECK_NO_THROW ( iteration ( hw.getNode("SUBSYSTEM1") ) );
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(NodeNavigationTestSuite, write_read, DummyHardwareFixture,
{
  HwInterface hw = getHwInterface();

  BOOST_CHECK_EQUAL ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getAddress(),
                      hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getAddress() );
  BOOST_CHECK_EQUAL ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getMask(),
                      hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getMask() );
  BOOST_CHECK_EQUAL ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getId(),
                      hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getId() );
  BOOST_CHECK_EQUAL ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getTags(),
                      hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getTags() );
  BOOST_CHECK_EQUAL ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.MEM" ).getMode(),
                      hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "MEM" ).getMode() );
  BOOST_CHECK_EQUAL ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.MEM" ).getSize(),
                      hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "MEM" ).getSize() );
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).write ( x );
  ValWord< uint32_t > reg = hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).read();
  hw.dispatch();
  BOOST_CHECK_EQUAL ( reg.value(), x );
}
)


UHAL_TESTS_DEFINE_CLIENT_TEST_CASES(NodeNavigationTestSuite, empty_node_id, MinimalFixture,
{
  HwInterface hw = getHwInterface();

  BOOST_CHECK_EQUAL ( &hw.getNode ( "" ), &hw.getNode() );
  BOOST_CHECK_EQUAL ( &hw.getNode ( "SUBSYSTEM1" ).getNode ( "" ), &hw.getNode ( "SUBSYSTEM1" ) );
}
)


} // end ns tests
} // end ns uhal
