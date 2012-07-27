#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <boost/filesystem.hpp>

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
	std::cout << "getNodes(): ";
	std::copy(ids.begin(),
		  ids.end(),
		  std::ostream_iterator<std::string>(std::cout,", "));
	
	std::cout << std::endl;
	
	ids = hw.getNodes(".*MEM.*");
	std::cout << "getNodes(\".*MEM.*\"): ";
	std::copy(ids.begin(),
		  ids.end(),
		  std::ostream_iterator<std::string>(std::cout,", "));
	
	std::cout << std::endl;

	ids = hw.getNode("SUBSYSTEM1").getNodes();
	std::cout << "getNode(\"SUBSYSTEM1\").getNodes(): ";
	std::copy(ids.begin(),
		  ids.end(),
		  std::ostream_iterator<std::string>(std::cout,", "));
	
	std::cout << std::endl;
	
}


int main ( int argc,char* argv[] )
{
	std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
	std::string connection_file = params["connection_file"];
	std::string device_id = params["device_id"];
	std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
	CACTUS_TEST ( navigation_and_traversal ( connection_file,device_id ) );

	return 0;
}
