#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <vector>
#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

#define N_1kB    1024/4
#define N_100kB  100*1024/4
#define N_10MB   10*1024*1024/4

void write_read_hierarchy(const std::string& connection, const std::string& id) {

}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing(argc,argv);
  
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  
  CACTUS_TEST(write_read_hierarchy(connection_file,device_id));

  return 0;
}
