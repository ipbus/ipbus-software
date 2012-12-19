#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;




void check_timeout ( const std::string& connection, const std::string& id, int sleepAfterFirstDispatch )
{
  ConnectionManager manager ( connection );
  HwInterface hw = manager.getDevice ( id );
  // Check we get an exception when first packet timeout occurs (dummy hardware only has delay on first packet)
  CACTUS_TEST_THROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } , uhal::exception );
  std::cout << "Sleeping for " << sleepAfterFirstDispatch << " seconds to allow DummyHardware to clear itself" << std::endl;
  sleep ( sleepAfterFirstDispatch );
  // Check we can continue as normal without further exceptions.
  CACTUS_TEST_NOTHROW ( { hw.getNode ( "REG" ).read();  hw.dispatch(); } );
}

/*
void check_timeout_setting ( const std::string& connection, const std::string& id, int sleepAfterFirstDispatch )
{
  ConnectionManager manager ( connection );
  HwInterface hw = manager.getDevice ( id );
  std::cout << "Checking timeout set correctly : Default value" << std::endl;
  hw.getNode ( "REG" ).read();
  log ( Info() , ThisLocation() , " : mTimeOut = " , Integer ( hw.getClient().getTimeoutPeriod() ) );
  hw.dispatch();
  std::cout << "Checking timeout set correctly : Set to 10ms" << std::endl;
  hw.getClient().setTimeoutPeriod ( 10 );
  hw.getNode ( "REG" ).read();
  log ( Info() , ThisLocation() , " : mTimeOut = " , Integer ( hw.getClient().getTimeoutPeriod() ) );
  hw.dispatch();
  std::cout << "Checking timeout set correctly : Set to pos_infinity" << std::endl;
  hw.getClient().setTimeoutPeriod ( 0 );
  hw.getNode ( "REG" ).read();
  log ( Info() , ThisLocation() , " : mTimeOut = " , Integer ( hw.getClient().getTimeoutPeriod() ) );
  hw.dispatch();
}
*/

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  CACTUS_TEST ( check_timeout ( connection_file, device_id, 3 ) );
  //check_timeout_setting ( connection_file, device_id, 3 );
  return 0;
}
