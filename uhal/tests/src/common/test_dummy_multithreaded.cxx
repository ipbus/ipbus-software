#include "uhal/uhal.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

#define N_THREADS     10
#define N_ITERATIONS  100
#define N_SIZE        1024*1024/4
#define TIMEOUT_S     50

void job_multiple ( const std::string& connection, const std::string& id )
{
  CACTUS_TEST_NOTHROW(
    for ( size_t iter=0; iter!= N_ITERATIONS ; ++iter )
      {
	ConnectionManager manager ( connection );
	HwInterface hw=manager.getDevice ( id );
	uint32_t x = static_cast<uint32_t> ( rand() );
	hw.getNode ( "REG" ).write ( x );
	ValWord< uint32_t > reg = hw.getNode ( "REG" ).read();
	std::vector<uint32_t> xx;
	
	for ( size_t i=0; i!= N_SIZE; ++i )
	  {
	    xx.push_back ( static_cast<uint32_t> ( rand() ) );
	  }
	
	hw.getNode ( "MEM" ).writeBlock ( xx );
	ValVector< uint32_t > mem = hw.getNode ( "MEM" ).readBlock ( N_SIZE );
	hw.dispatch();
	CACTUS_CHECK ( reg.valid() );
	CACTUS_CHECK ( mem.valid() );
	CACTUS_CHECK ( mem.size() == N_SIZE );
	//can not check content in the mutlithreaded case
      }
    );
}

void multiple_hwinterfaces ( const std::string& connection_file,const std::string& device_id )
{
  std::vector<boost::thread*> jobs;

  for ( size_t i=0; i!=N_THREADS; ++i )
  {
    jobs.push_back ( new boost::thread ( job_multiple,connection_file,device_id ) );
  }

  for ( size_t i=0; i!=N_THREADS; ++i )
  {
    //boost::posix_time::time_duration timeout = boost::posix_time::seconds ( TIMEOUT_S );
    //CACTUS_CHECK ( jobs[i]->timed_join ( timeout ) );
    jobs[i]->join();
    delete jobs[i];
  }
}

void job_single ( HwInterface& hw )
{
  CACTUS_TEST_NOTHROW(
    uint32_t x = static_cast<uint32_t> ( rand() );
    hw.getNode ( "REG" ).write ( x );
    ValWord< uint32_t > reg = hw.getNode ( "REG" ).read();
    std::vector<uint32_t> xx;
    
    for ( size_t i=0; i!= N_SIZE; ++i )
      {
	xx.push_back ( static_cast<uint32_t> ( rand() ) );
      }
    
    hw.getNode ( "MEM" ).writeBlock ( xx );
    ValVector< uint32_t > mem = hw.getNode ( "MEM" ).readBlock ( N_SIZE );

    hw.dispatch();
    
    CACTUS_CHECK ( reg.valid() );
    CACTUS_CHECK ( mem.valid() );
    CACTUS_CHECK ( mem.size() == N_SIZE );
    );
  //can not check content in the mutlithreaded case
}

void single_hwinterface ( const std::string& connection_file,const std::string& device_id )
{
  for ( size_t iter=0; iter!= N_ITERATIONS ; ++iter )
  {
    ConnectionManager manager ( connection_file );
    HwInterface hw=manager.getDevice ( device_id );

    std::vector<boost::thread*> jobs;
    
    for ( size_t i=0; i!=N_THREADS; ++i )
      {
	jobs.push_back ( new boost::thread ( job_single,hw ) );
      }
    
    for ( size_t i=0; i!=N_THREADS; ++i )
      {
	jobs[i]->join();
	delete jobs[i];
      }
  }
}

void job_single_copied ( HwInterface hw )
{
  CACTUS_TEST_NOTHROW(
    uint32_t x = static_cast<uint32_t> ( rand() );
    hw.getNode ( "REG" ).write ( x );
    ValWord< uint32_t > reg = hw.getNode ( "REG" ).read();
    std::vector<uint32_t> xx;
    
    for ( size_t i=0; i!= N_SIZE; ++i )
      {
	xx.push_back ( static_cast<uint32_t> ( rand() ) );
      }

    hw.getNode ( "MEM" ).writeBlock ( xx );
    ValVector< uint32_t > mem = hw.getNode ( "MEM" ).readBlock ( N_SIZE );

    hw.dispatch();

    CACTUS_CHECK ( reg.valid() );
    CACTUS_CHECK ( mem.valid() );
    CACTUS_CHECK ( mem.size() == N_SIZE );
    );
}

void single_copied_hwinterface ( const std::string& connection_file,const std::string& device_id )
{
  for ( size_t iter=0; iter!= N_ITERATIONS ; ++iter )
  {
    ConnectionManager manager ( connection_file );
    HwInterface hw=manager.getDevice ( device_id );

    std::vector<boost::thread*> jobs;
    
    for ( size_t i=0; i!=N_THREADS; ++i )
      {
	jobs.push_back ( new boost::thread ( job_single_copied,hw ) );
      }
    
    for ( size_t i=0; i!=N_THREADS; ++i )
      {
	jobs[i]->join();
	delete jobs[i];
      }
  }
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  CACTUS_TEST ( multiple_hwinterfaces (connection_file,device_id ) );
  CACTUS_TEST ( single_hwinterface (connection_file,device_id ) );
  CACTUS_TEST ( single_copied_hwinterface (connection_file,device_id ) );
  return 0;
}
