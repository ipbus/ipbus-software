#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void connect_write_read(const std::string& connection, const std::string& id) {
  ConnectionManager manager(connection);

  HwInterface hw=manager.getDevice(id);
  
  hw.ping();

  uint32_t x = static_cast<uint32_t>(rand());
  
  hw.getNode("REG").write(x);
  ValWord< uint32_t > mem = hw.getNode("REG").read();
  
  CACTUS_CHECK(!mem.valid());
  CACTUS_TEST_THROW(mem.value(),uhal::exception);
  //CACTUS_TEST_THROW(mem.value(),uhal::NonValidatedMemory); --> https://svnweb.cern.ch/trac/cactus/ticket/32
  
  CACTUS_TEST(hw.dispatch());
  
  CACTUS_CHECK(mem.valid());
  CACTUS_CHECK(mem.value() == x);
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing(argc,argv);
  
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  
  CACTUS_TEST(connect_write_read(connection_file,device_id));
  
  return 0;
}
