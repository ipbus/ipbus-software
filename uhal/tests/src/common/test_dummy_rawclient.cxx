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
  const uint32_t N =1024*1024/4;

  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  boost::shared_ptr<ClientInterface> c = hw.getClient();

  std::vector<uint32_t> xx;

  for ( size_t i=0; i!= N; ++i )  {
      xx.push_back ( static_cast<uint32_t> ( rand() ) );
    }

  uint32_t addr = hw.getNode ( "MEM" ).getAddress();
  c->writeBlock ( addr, xx );
  ValVector< uint32_t > mem = c->readBlock (addr, N );
	
  CACTUS_CHECK ( !mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  CACTUS_TEST_THROW ( mem.at ( 0 ),uhal::NonValidatedMemory );

  c->dispatch();

  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  bool correct_block_write_read = true;
  ValVector< uint32_t >::const_iterator i=mem.begin();
  std::vector< uint32_t >::const_iterator j=xx.begin();

  for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )  {
      correct_block_write_read = correct_block_write_read && ( *i == *j );
    }

  CACTUS_CHECK ( correct_block_write_read );
}

void mem_rmw_bits ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  boost::shared_ptr<ClientInterface> c = hw.getClient();
	
  uint32_t addr = hw.getNode ( "REG_UPPER_MASK" ).getAddress();
  uint32_t x1 = static_cast<uint32_t> ( rand() );
  uint32_t x2 = static_cast<uint32_t> ( rand() );
  uint32_t x3 = static_cast<uint32_t> ( rand() );

  c->write(addr,x1);
  ValWord< uint32_t > reg1 = c->rmw_bits(addr,x2,x3);
  ValWord< uint32_t > reg2 = c->read(addr);

  c->dispatch();
	
  CACTUS_CHECK(reg1.value() == reg2.value());
  CACTUS_CHECK(reg1.value() == ((x1 & x2)| x3));
	
}

void mem_rmw_sum ( const std::string& connection, const std::string& id )
{
  const uint32_t N =1024;
	
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  boost::shared_ptr<ClientInterface> c = hw.getClient();
  
  uint32_t total = 0;
  std::vector<uint32_t> xx;
  for ( size_t i=0; i!= N; ++i ) {
    uint32_t x = static_cast<uint32_t> ( rand() );
    total += x;
    xx.push_back ( x );
  }
	
  uint32_t addr = hw.getNode ( "SUBSYSTEM1.REG" ).getAddress();
  c->write(addr,xx[0]);

  ValWord<uint32_t> reg;
  for ( size_t i=1; i!= N; ++i ) {
    reg = c->rmw_sum(addr,xx[i]);
    c->dispatch();
 
  }

  CACTUS_CHECK(reg.value() == total);
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
