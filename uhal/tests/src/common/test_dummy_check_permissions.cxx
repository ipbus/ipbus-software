#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void check_permissions ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	CACTUS_TEST_THROW ( hw.getNode ( "REG_READ_ONLY" ).write ( 1 ),uhal::WriteAccessDenied );
	CACTUS_TEST_THROW ( hw.getNode ( "REG_WRITE_ONLY" ).read(),uhal::ReadAccessDenied );
	uint32_t x = static_cast<uint32_t> ( rand() );
	hw.getNode ( "REG_WRITE_ONLY" ).write ( x );
	ValWord< uint32_t > mem = hw.getNode ( "REG_READ_ONLY" ).read();
	hw.dispatch();
}

int main ( int argc,char* argv[] )
{
	std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
	std::string connection_file = params["connection_file"];
	std::string device_id = params["device_id"];
	std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
	CACTUS_TEST ( check_permissions ( connection_file,device_id ) );
	return 0;
}
