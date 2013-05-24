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

#include <boost/filesystem.hpp>

#include <vector>
#include <algorithm>
#include <string>
#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void connect_write_read ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  // hw.ping();
  uint32_t x1 = static_cast<uint32_t> ( rand() );
  uint32_t x2 = static_cast<uint32_t> ( rand() );
  hw.getNode ( "SUBSYSTEM1.REG" ).write ( x1 );
  hw.getNode ( "SUBSYSTEM2.REG" ).write ( x2 );
  ValWord< uint32_t > mem1 = hw.getNode ( "SUBSYSTEM1.REG" ).read();
  ValWord< uint32_t > mem2 = hw.getNode ( "SUBSYSTEM2.REG" ).read();
  CACTUS_CHECK ( !mem1.valid() );
  CACTUS_CHECK ( !mem2.valid() );
  CACTUS_TEST_THROW ( mem1.value(),uhal::exception::NonValidatedMemory );
  CACTUS_TEST_THROW ( mem2.value(),uhal::exception::NonValidatedMemory );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem1.valid() );
  CACTUS_CHECK ( mem1.value() == x1 );
  CACTUS_CHECK ( mem2.value() == x2 );
}

void on_the_fly_connect_write_read ( const std::string& connection, const std::string& id )
{
  //get location of address file. Assumption: it is located with the connection file
  std::string address_file;
  {
    boost::filesystem::path conn_fn ( connection );
    boost::filesystem::path fn ( "dummy_address.xml" );
    address_file = ( conn_fn.parent_path() /fn ).string();
  }
  //get the parameters from the file
  std::string uri;
  {
    ConnectionManager manager ( connection );
    uri = manager.getDevice ( id ).uri();
  }
  HwInterface hw=ConnectionManager::getDevice ( "test_device_id",uri,address_file );
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG" ).write ( x );
  ValWord< uint32_t > mem = hw.getNode ( "REG" ).read();
  CACTUS_CHECK ( !mem.valid() );
  CACTUS_TEST_THROW ( mem.value(),uhal::exception::NonValidatedMemory );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.value() == x );
}

void search_device_id ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  std::vector<std::string> ids = manager.getDevices ( "^" + id + "$" );
  CACTUS_CHECK ( std::find ( ids.begin(),ids.end(),id ) != ids.end() );
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  CACTUS_TEST ( connect_write_read ( connection_file,device_id ) );
  CACTUS_TEST ( on_the_fly_connect_write_read ( connection_file,device_id ) );
  CACTUS_TEST ( search_device_id ( connection_file,device_id ) );
  CACTUS_TEST_RESULT();
}
