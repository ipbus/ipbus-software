#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <ios>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void write_read_masked ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG_LOWER_MASK" ).write ( x && 0xFFFF );
  hw.getNode ( "REG_UPPER_MASK" ).write ( x >> 16 );
  CACTUS_TEST_THROW ( hw.getNode ( "REG_LOWER_MASK" ).write ( 0x1FFFF ),uhal::exception );
  CACTUS_TEST_THROW ( hw.getNode ( "REG_UPPER_MASK" ).write ( 0x1FFFF ),uhal::exception );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getMask() == 0xFFFF );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getMask() == 0xFFFF0000 );
  ValWord<uint32_t> reg_l = hw.getNode ( "REG_LOWER_MASK" ).read();
  ValWord<uint32_t> reg_u = hw.getNode ( "REG_UPPER_MASK" ).read();
  CACTUS_CHECK ( !reg_l.valid() && !reg_u.valid() );
  CACTUS_TEST_THROW ( reg_l.value(),uhal::exception );
  CACTUS_TEST_THROW ( reg_u.value(),uhal::exception );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( reg_l.value() <= 0xFFFF );
  CACTUS_CHECK ( reg_u.value() <= 0xFFFF );
  CACTUS_CHECK ( reg_l.valid() && reg_u.valid() );
  CACTUS_CHECK ( reg_l.value() == ( x && 0xFFFF ) );
  CACTUS_CHECK ( reg_u.value() == ( x >> 16 ) );
}



int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  CACTUS_TEST ( write_read_masked ( connection_file,device_id ) );
  return 0;
}
