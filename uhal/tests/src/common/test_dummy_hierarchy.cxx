#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <vector>
#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

#define N    1024*1024/4

void write_read_hierarchy ( const std::string& connection, const std::string& id )
{
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	//check non-overlapping addresses
	CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getAddress() != hw.getNode ( "SUBSYSTEM2.REG" ).getAddress() );
	CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getAddress() != hw.getNode ( "SUBSYSTEM2.MEM" ).getAddress() );
	//create transactions
	uint32_t x1 = static_cast<uint32_t> ( rand() );
	hw.getNode ( "SUBSYSTEM1.REG" ).write ( x1 );
	ValWord< uint32_t > reg1 = hw.getNode ( "SUBSYSTEM1.REG" ).read();
	uint32_t x2 = static_cast<uint32_t> ( rand() );
	hw.getNode ( "SUBSYSTEM2.REG" ).write ( x2 );
	ValWord< uint32_t > reg2 = hw.getNode ( "SUBSYSTEM2.REG" ).read();
	std::vector<uint32_t> xx1;

	for ( size_t i=0; i!= N; ++i )
	{
		xx1.push_back ( static_cast<uint32_t> ( rand() ) );
	}

	hw.getNode ( "SUBSYSTEM1.MEM" ).writeBlock ( xx1 );
	ValVector< uint32_t > mem1 = hw.getNode ( "SUBSYSTEM1.MEM" ).readBlock ( N );
	std::vector<uint32_t> xx2;

	for ( size_t i=0; i!= N; ++i )
	{
		xx2.push_back ( static_cast<uint32_t> ( rand() ) );
	}

	hw.getNode ( "SUBSYSTEM2.MEM" ).writeBlock ( xx2 );
	ValVector< uint32_t > mem2 = hw.getNode ( "SUBSYSTEM2.MEM" ).readBlock ( N );
	//check some preconditions
	CACTUS_CHECK ( !reg1.valid() && !reg2.valid() && !mem1.valid() && !mem2.valid() );
	CACTUS_CHECK ( mem1.size() == N );
	CACTUS_CHECK ( mem2.size() == N );
	CACTUS_TEST_THROW ( mem1.at ( rand() % N ),uhal::exception );
	CACTUS_TEST_THROW ( mem2.at ( rand() % N ),uhal::exception );
	//send packet
	CACTUS_TEST ( hw.dispatch() );
	//check results
	CACTUS_CHECK ( reg1.value() == x1 );
	bool correct_block_write_read_subsystem1 = true;
	ValVector< uint32_t >::const_iterator i1=mem1.begin();
	std::vector< uint32_t >::const_iterator j1=xx1.begin();

	for ( ; i1!=mem1.end(); ++i1 , ++j1 )
	{
		correct_block_write_read_subsystem1 = correct_block_write_read_subsystem1 && ( *i1 == *j1 );
	}

	CACTUS_CHECK ( mem1.size() == N );
	CACTUS_CHECK ( correct_block_write_read_subsystem1 );
	CACTUS_CHECK ( reg2.value() == x2 );
	bool correct_block_write_read_subsystem2 = true;
	ValVector< uint32_t >::const_iterator i2=mem2.begin();
	std::vector< uint32_t >::const_iterator j2=xx2.begin();

	for ( ; i2!=mem2.end(); ++i2 , ++j2 )
	{
		correct_block_write_read_subsystem2 = correct_block_write_read_subsystem2 && ( *i2 == *j2 );
	}

	CACTUS_CHECK ( mem2.size() == N );
	CACTUS_CHECK ( correct_block_write_read_subsystem2 );
}

int main ( int argc,char* argv[] )
{
	std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
	std::string connection_file = params["connection_file"];
	std::string device_id = params["device_id"];
	std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
	CACTUS_TEST ( write_read_hierarchy ( connection_file,device_id ) );
	return 0;
}
