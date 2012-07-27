#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <vector>
#include <string>
#include <iostream>
#include <cstdlib>
#include <typeinfo>
#include <iterator>

using namespace uhal;

void navigation_and_traversal ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );

	std::vector<std::string> ids = hw.getNodes();
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM1.REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM1.MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM2.REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM2.MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.MEM") != ids.end());

	ids = hw.getNodes ( ".*MEM.*" );
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"LARGE_MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SMALL_MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM1.MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM2.MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM1.SUBMODULE.MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBSYSTEM2.SUBMODULE.MEM") != ids.end());

	ids = hw.getNode ( "SUBSYSTEM1" ).getNodes();
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"MEM") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBMODULE.REG") != ids.end());
	CACTUS_CHECK(std::find(ids.begin(),ids.end(),"SUBMODULE.MEM") != ids.end());
}

void write_read ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	
	CACTUS_CHECK(hw.getNode("SUBSYSTEM1.SUBMODULE.REG").getAddress()
		     == 
		     hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getAddress());
	CACTUS_CHECK(hw.getNode("SUBSYSTEM1.SUBMODULE.REG").getMask()
		     == 
		     hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getMask());
	CACTUS_CHECK(hw.getNode("SUBSYSTEM1.SUBMODULE.REG").getId()
		     == 
		     hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getId());
	CACTUS_CHECK(hw.getNode("SUBSYSTEM1.SUBMODULE.REG").getTags()
		     == 
		     hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).getTags());
	CACTUS_CHECK(hw.getNode("SUBSYSTEM1.SUBMODULE.MEM").getMode()
		     == 
		     hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "MEM" ).getMode());
	CACTUS_CHECK(hw.getNode("SUBSYSTEM1.SUBMODULE.MEM").getSize()
		     == 
		     hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "MEM" ).getSize());

	uint32_t x = static_cast<uint32_t> ( rand() );
	hw.getNode("SUBSYSTEM1.SUBMODULE.REG").write ( x );
	ValWord< uint32_t > reg = hw.getNode ( "SUBSYSTEM1" ).getNode ( "SUBMODULE" ).getNode ( "REG" ).read();
	
	hw.dispatch();

	CACTUS_CHECK ( reg.value() == x );
}
	
int main ( int argc,char* argv[] )
{
	std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
	std::string connection_file = params["connection_file"];
	std::string device_id = params["device_id"];
	std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
	CACTUS_TEST ( navigation_and_traversal ( connection_file,device_id ) );
	CACTUS_TEST ( write_read ( connection_file,device_id ) );

	return 0;
}
