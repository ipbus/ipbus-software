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

#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <vector>
#include <string>
#include <iostream>
#include <cstdlib>
#include <typeinfo>
#include <iterator>

using namespace uhal;

void iteration ( const uhal::Node& parentNode )
{ 
  uint32_t lAddr = 0x0;

  for(uhal::Node::const_iterator lIt = parentNode.begin(); lIt != parentNode.end(); lIt++)
  {
    CACTUS_CHECK ( lIt->getAddress() >= lAddr );
    lAddr = lIt->getAddress();
  }
}

void navigation_and_traversal ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  std::vector<std::string> ids = hw.getNodes();
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.MEM" ) != ids.end() );
  ids = hw.getNodes ( ".*MEM.*" );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"LARGE_MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SMALL_MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.MEM" ) != ids.end() );
  ids = hw.getNode ( "SUBSYSTEM1" ).getNodes();
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"MEM" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBMODULE.REG" ) != ids.end() );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),"SUBMODULE.MEM" ) != ids.end() );

  CACTUS_TEST ( iteration ( hw.getNode() ) );
  CACTUS_TEST ( iteration ( hw.getNode("SUBSYSTEM1") ) );
}

void write_read ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getAddress()
                 ==
                 hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getAddress() );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getMask()
                 ==
                 hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getMask() );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getId()
                 ==
                 hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getId() );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).getTags()
                 ==
                 hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getTags() );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.MEM" ).getMode()
                 ==
                 hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "MEM" ).getMode() );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.SUBMODULE.MEM" ).getSize()
                 ==
                 hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "MEM" ).getSize() );
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "SUBSYSTEM1.SUBMODULE.REG" ).write ( x );
  ValWord< uint32_t > reg = hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).read();
  hw.dispatch();
  CACTUS_CHECK ( reg.value() == x );
}


void empty_node_id ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  CACTUS_CHECK ( &hw.getNode ( "" ) == &hw.getNode() );
  CACTUS_CHECK ( &hw.getNode ( "SUBSYSTEM1" ).getNode ( "" ) == & hw.getNode ( "SUBSYSTEM1" ) );
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  CACTUS_TEST ( navigation_and_traversal ( connection_file,device_id ) );
  CACTUS_TEST ( write_read ( connection_file,device_id ) );
  CACTUS_TEST ( empty_node_id ( connection_file,device_id ) );
  CACTUS_TEST_RESULT();
}
