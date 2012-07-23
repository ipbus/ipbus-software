#include "uhal/uhal.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

#define N_THREADS     10
#define N_ITERATIONS  100
#define N_SIZE        1024*1024/4
#define TIMEOUT_S     50

void job ( const std::string& connection, const std::string& id )
{
  for ( size_t iter=0; iter!= N_ITERATIONS ; ++iter )
    {
      std::cout << "thread = " << boost::this_thread::get_id() << std::endl;
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
      CACTUS_TEST ( hw.dispatch() );
      CACTUS_CHECK ( reg.value() == x );
      CACTUS_CHECK ( mem.valid() );
      CACTUS_CHECK ( mem.size() == N_SIZE );
      bool correct_block_write_read = true;
      ValVector< uint32_t >::const_iterator i=mem.begin();
      std::vector< uint32_t >::const_iterator j=xx.begin();

      for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
	{
	  correct_block_write_read = correct_block_write_read && ( *i == *j );
	}

      CACTUS_CHECK ( correct_block_write_read );
    }
}

void launch_threads ( size_t n_threads,const std::string& connection_file,const std::string& device_id )
{
  std::vector<boost::thread*> jobs;

  for ( size_t i=0; i!=n_threads; ++i )
    {
      jobs.push_back ( new boost::thread ( job,connection_file,device_id ) );
    }

  for ( size_t i=0; i!=n_threads; ++i )
    {
      //boost::posix_time::time_duration timeout = boost::posix_time::seconds ( TIMEOUT_S );
      //CACTUS_CHECK ( jobs[i]->timed_join ( timeout ) );
      CACTUS_TEST_NOTHROW ( jobs[i]->join() );
      delete jobs[i];
    }
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] << " (connection_file='" << connection_file<<"', device_id='" << device_id << "')..." << std::endl;
  CACTUS_TEST ( launch_threads ( N_THREADS,connection_file,device_id ) );
  return 0;
}
