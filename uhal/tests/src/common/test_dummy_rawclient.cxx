#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <boost/shared_ptr.hpp>

#include <vector>
#include <algorithm>
#include <string>
#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void single_write_read ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	boost::shared_ptr<ClientInterface> c = hw.getClient();

	uint32_t x = static_cast<uint32_t> ( rand() );
	uint32_t addr = hw.getNode ( "REG" ).getAddress();
	
	c->write(addr,x);
	ValWord< uint32_t > reg = c->read(addr);
	
	CACTUS_CHECK ( !reg.valid() );
	CACTUS_TEST_THROW(reg.value(),uhal::NonValidatedMemory);
	
	c->dispatch();
	
	CACTUS_CHECK ( reg.valid() );
	CACTUS_CHECK(reg.value() == x);
}

void mem_write_read ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	boost::shared_ptr<ClientInterface> c = hw.getClient();
}

void mem_rmw_bits ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	boost::shared_ptr<ClientInterface> c = hw.getClient();
}

void mem_rmw_sum ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	boost::shared_ptr<ClientInterface> c = hw.getClient();
}

int main ( int argc,char* argv[] )
{
	std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
	std::string connection_file = params["connection_file"];
	std::string device_id = params["device_id"];
	std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
	CACTUS_TEST ( single_write_read ( connection_file,device_id ) );
	CACTUS_TEST ( mem_write_read ( connection_file,device_id ) );
	CACTUS_TEST ( mem_rmw_bits ( connection_file,device_id ) );
	CACTUS_TEST ( mem_rmw_sum ( connection_file,device_id ) );

	return 0;
}
