#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <boost/filesystem.hpp>

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void check_meta_info ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	
	//REG
	CACTUS_CHECK(hw.getNode ( "REG" ).getAddress() == 0x0001);
	CACTUS_CHECK(hw.getNode ( "REG" ).getId() == "REG");
	CACTUS_CHECK(hw.getNode ( "REG" ).getPermission() == (uhal::defs::READ |uhal::defs::WRITE) );
	std::cout << hw.getNode ( "REG" ).getSize() << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG" ).getSize() == 1);
	CACTUS_CHECK(hw.getNode ( "REG" ).getMask() == uhal::defs::NOMASK);
	CACTUS_CHECK(hw.getNode ( "REG" ).getMode() == uhal::defs::SINGLE);
	std::cout << "'" << hw.getNode ( "REG" ).getTags() << "'" << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG" ).getTags() == "test");

	
	//REG_READ_ONLY
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getAddress() == 0x0002);
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getId() == "REG_READ_ONLY");
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getPermission() == uhal::defs::READ );
	std::cout << hw.getNode ( "REG_READ_ONLY" ).getSize() << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getSize() == 1);
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getMask() == uhal::defs::NOMASK);
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getMode() == uhal::defs::SINGLE);
	std::cout << "'" << hw.getNode ( "REG_READ_ONLY" ).getTags() << "'" << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_READ_ONLY" ).getTags() == "");

	//REG_WRITE_ONLY
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getAddress() == 0x0003);
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getId() == "REG_WRITE_ONLY");
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getPermission() == uhal::defs::WRITE );
	std::cout << hw.getNode ( "REG_WRITE_ONLY" ).getSize() << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getSize() == 1);
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getMask() == uhal::defs::NOMASK);
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getMode() == uhal::defs::SINGLE);
	std::cout << "'" << hw.getNode ( "REG_WRITE_ONLY" ).getTags() << "'" << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_WRITE_ONLY" ).getTags() == "");

	//REG_UPPER_MASK
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getAddress() == 0x0004);
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getId() == "REG_UPPER_MASK");
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getPermission() == uhal::defs::READWRITE );
	std::cout << hw.getNode ( "REG_UPPER_MASK" ).getSize() << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getSize() == 1);
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getMask() == 0xFFFF0000);
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getMode() == uhal::defs::SINGLE);
	std::cout << "'" << hw.getNode ( "REG_UPPER_MASK" ).getTags() << "'" << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_UPPER_MASK" ).getTags() == "");
	
	//REG_LOWER_MASK
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getAddress() == 0x0004);
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getId() == "REG_LOWER_MASK");
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getPermission() == uhal::defs::READWRITE );
	std::cout << hw.getNode ( "REG_LOWER_MASK" ).getSize() << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getSize() == 1);
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getMask() == 0x0000FFFF);
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getMode() == uhal::defs::SINGLE);
	std::cout << "'" << hw.getNode ( "REG_LOWER_MASK" ).getTags() << "'" << std::endl;
	CACTUS_CHECK(hw.getNode ( "REG_LOWER_MASK" ).getTags() == "");

}

int main ( int argc,char* argv[] )
{
	std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
	std::string connection_file = params["connection_file"];
	std::string device_id = params["device_id"];
	std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
	check_meta_info( connection_file,device_id );
	return 0;
}
