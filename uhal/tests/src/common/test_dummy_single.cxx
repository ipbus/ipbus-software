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
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG" ).write ( x );
  ValWord< uint32_t > mem = hw.getNode ( "REG" ).read();
  CACTUS_CHECK ( !mem.valid() );
  CACTUS_TEST_THROW ( mem.value(),uhal::NonValidatedMemory );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.value() == x );
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
  CACTUS_TEST_THROW ( mem.value(),uhal::NonValidatedMemory );
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
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  CACTUS_TEST ( connect_write_read ( connection_file,device_id ) );
  CACTUS_TEST ( on_the_fly_connect_write_read ( connection_file,device_id ) );
  CACTUS_TEST ( search_device_id ( connection_file,device_id ) );
  return 0;
}
