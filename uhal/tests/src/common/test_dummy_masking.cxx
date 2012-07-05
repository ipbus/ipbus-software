#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <ios>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void write_read_masked(const std::string& connection, const std::string& id) {
  ConnectionManager manager(connection);

  HwInterface hw=manager.getDevice(id);
  
  hw.ping();

  uint32_t x1 = static_cast<uint32_t>(rand());
  uint32_t x2 = static_cast<uint32_t>(rand());

  hw.getNode("REG_WITHOUT_MASK").write(x1);
  ValWord< uint32_t > mem1 = hw.getNode("REG_WITHOUT_MASK").read();
  ValWord< uint32_t > mem2 = hw.getNode("REG_WITH_MASK").read();

  hw.getNode("REG_WITH_MASK").write(x2);
  ValWord< uint32_t > mem3 = hw.getNode("REG_WITHOUT_MASK").read();
  ValWord< uint32_t > mem4 = hw.getNode("REG_WITH_MASK").read();
  
  CACTUS_CHECK(!mem1.valid() && !mem2.valid() && !mem3.valid() && !mem4.valid() );
  CACTUS_TEST_THROW(mem1.value(),uhal::exception);
  CACTUS_TEST_THROW(mem2.value(),uhal::exception);
  CACTUS_TEST_THROW(mem3.value(),uhal::exception);
  CACTUS_TEST_THROW(mem4.value(),uhal::exception);  
  //CACTUS_TEST_THROW(mem.value(),uhal::NonValidatedMemory); --> https://svnweb.cern.ch/trac/cactus/ticket/32
  
  CACTUS_TEST(hw.dispatch()); 
  CACTUS_CHECK(mem1.valid() && mem2.valid() && mem3.valid() && mem4.valid() );

  std::cout << std::setfill('0') << std::hex;
  std::cout << "Written first, unmasked : x1 = 0x" << std::setw(8) << x1 << std::endl;
  std::cout << "Read back, unmasked : mem1 = 0x" << std::setw(8) << mem1.value() << std::endl;
  std::cout << "Read back, masked : mem2 = 0x" << std::setw(8) << mem2.value() << std::endl;
  std::cout << "Written second, masked with 0xFFFF and left shifted by 8 bits : x2 = 0x" << std::setw(8) << x2 << std::endl;
  std::cout << "Read back, unmasked : mem3 = 0x" << std::setw(8) << mem3.value() << std::endl;
  std::cout << "Read back, masked : mem4 = 0x" << std::setw(8) << mem4.value() << std::endl;
  std::cout << std::dec;
  
  CACTUS_CHECK( mem1.value() == x1 );
  CACTUS_CHECK( mem2.value() == ( ( x1 >> 8 ) & 0xffff ) );
  CACTUS_CHECK( mem3.value() == ( ( x1 & 0xff0000ff ) | ( ( x2 & 0xffff ) << 8 ) ) );
  CACTUS_CHECK( mem4.value() == ( x2 & 0xffff ) );
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing(argc,argv);
  
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  
  CACTUS_TEST(write_read_masked(connection_file,device_id));
  
  return 0;
}
