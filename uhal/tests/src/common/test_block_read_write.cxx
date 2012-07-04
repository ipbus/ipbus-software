#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void block_write_and_read(const std::string& connection, const std::string& id) {
  ConnectionManager manager(connection);

  HwInterface hw=manager.getDevice(id);

}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing(argc,argv);
  
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  
  CACTUS_TEST(block_write_and_read(connection_file,device_id));
  
  return 0;
}
