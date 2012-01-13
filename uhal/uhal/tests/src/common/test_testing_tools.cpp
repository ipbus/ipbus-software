#define BOOST_TEST_DYN_LINK

#define BOOST_TEST_MODULE Test for the test utils 

#include <boost/test/unit_test.hpp>

#include "uhal/test_tools.hpp"

#include <string>
#include <cstdlib>

//usage: test_testing_tools --host=a.b.c --port=87 --addr=$[0x32]
BOOST_AUTO_TEST_CASE(retrieve_test_parameters) {
  std::string host = uhal::test::getParam<std::string>("host","");
  int port = uhal::test::getParam<int>("port",0);
  uint32_t addr = uhal::test::getParam<uint32_t>("addr",0);

  BOOST_CHECK(host != "unknown");
  BOOST_CHECK(port != 0);
  BOOST_CHECK(addr != 0);
  
  if (host == "" || port == 0 || addr==0) {
    std::cout << "Did you forgot to set the parameters?" << std::endl;
    std::cout << "usage: " << boost::unit_test::framework::master_test_suite().argv[0] << " --host=<host> --port=<port> --addr=<addr>" << std::endl;
    std::cout << "Where <addr> can use hex format using bash (e.g. <addr>=$[0x32])" << std::endl;
  }
}
