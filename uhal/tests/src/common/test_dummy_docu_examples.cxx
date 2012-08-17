#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <vector>
#include <iostream>

using namespace uhal;

void test_docu_addr_table_examples ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );

  // This line is majority of the test (i.e. load the addr table without exception).
  HwInterface hw=manager.getDevice ( id );


  // ***** "Single Register Address Table" example *****
  CACTUS_TEST_NOTHROW
  (
    ValWord< uint32_t > reg = hw.getNode ("A").read();
    hw.dispatch();
  );


  // ***** "Single Register on a Hierarchical Address Table" *****
  CACTUS_TEST_NOTHROW
  (
    //This is equivalent to getNode("B.A")
    ValWord< uint32_t > reg = hw.getNode("B").getNode("A").read();
    hw.dispatch();
  );

  
  // ***** "Multiple Modules with Identical Structure" *****
  CACTUS_TEST_NOTHROW
  (
    ValWord< uint32_t > reg = hw.getNode("D1.A2").read();
    hw.dispatch();
  );


  // ***** "Read and Write Blocks of Memory and FIFOs" *****
  CACTUS_TEST_NOTHROW
  (
    //read
    ValVector< uint32_t > mem = hw.getNode("F.A3").readBlock(16);
    ValVector< uint32_t > fifo = hw.getNode("F.A6").readBlock(16);

    //write
    std::vector<uint32_t> x;
    //fill x...
    for(unsigned int iFill = 0 ; iFill < 16 ; ++iFill) { x.push_back(iFill); }

    hw.getNode("F.A4").writeBlock(x);
    hw.getNode("F.A7").writeBlock(x);
    hw.dispatch();
  );
}

int main ( int argc, char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file <<"', device_id='" << device_id << "')..." << std::endl;
  CACTUS_TEST ( test_docu_addr_table_examples ( connection_file, device_id ) );
  return 0;
}

