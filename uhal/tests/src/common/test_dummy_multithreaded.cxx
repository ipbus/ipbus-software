/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

---------------------------------------------------------------------------
*/

#include "uhal/uhal.hpp"
#include "uhal/tests/tools.hpp"
#include "uhal/log/log.hpp"

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>

#include <iostream>
#include <cstdlib>
#include <typeinfo>

// Linux C++ headers
#include <sys/time.h>

using namespace uhal;

#define N_THREADS     5
#define N_ITERATIONS  5
#define N_SIZE        10*1024/4
#define TIMEOUT_S     50

void job_multiple ( const std::string& connection, const std::string& id )
{
  CACTUS_TEST_NOTHROW (

    for ( size_t iter=0; iter!= N_ITERATIONS ; ++iter )
{
  log ( Info() , "Iteration " , Integer ( iter ) );
    ConnectionManager manager ( connection );
    HwInterface hw=manager.getDevice ( id );
    hw.setTimeoutPeriod ( TIMEOUT_S*1000 );
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
    log ( Warning() , ThisLocation() );
    hw.dispatch();
    log ( Warning() , ThisLocation() );
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
    log ( Warning() , ThisLocation() , ":" , Integer ( i ) );
    jobs.push_back ( new boost::thread ( job_multiple,connection_file,device_id ) );
  }

  for ( size_t i=0; i!=N_THREADS; ++i )
  {
    log ( Warning() , ThisLocation() , ":" , Integer ( i ) );
    //boost::posix_time::time_duration timeout = boost::posix_time::seconds ( TIMEOUT_S );
    //CACTUS_CHECK ( jobs[i]->timed_join ( timeout ) );
    jobs[i]->join();
    delete jobs[i];
  }

  log ( Warning() , ThisLocation() );
}

void job_single ( HwInterface& hw )
{
  CACTUS_TEST_NOTHROW (
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
  CACTUS_TEST_NOTHROW (
    uint32_t x = static_cast<uint32_t> ( rand() );
    hw.getNode ( "REG" ).write ( x );
    ValWord< uint32_t > reg = hw.getNode ( "REG" ).read();
    std::vector<uint32_t> xx;

    for ( size_t i=0; i!= N_SIZE; ++i )
{
  xx.push_back ( static_cast<uint32_t> ( 0xDEADBEEF /*rand()*/ ) );
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
  CACTUS_TEST ( multiple_hwinterfaces ( connection_file,device_id ) );
  CACTUS_TEST ( single_hwinterface ( connection_file,device_id ) );
  CACTUS_TEST ( single_copied_hwinterface ( connection_file,device_id ) );
  CACTUS_TEST_RESULT();
}
