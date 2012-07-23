#include "uhal/uhal.hpp"
#include "uhal/tests/tools.hpp"

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp> 

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

#define MAX_THREADS   128
#define N_ITERATIONS  100
#define TIMEOUT_S     50  


void job(const size_t n_threads, const std::string& connection, const std::string& id) {
  ConnectionManager manager(connection);
  HwInterface hw=manager.getDevice(id);
  
  for(size_t iter=0; iter != N_ITERATIONS; ++iter) {

    ValWord< uint32_t > reg = hw.getNode("REG").read();
    
    timeval start,end;
    gettimeofday(&start,NULL);
    hw.dispatch();
    gettimeofday(&end,NULL);
    
    uhal::log ( Notice() ,Integer(n_threads), ", ", Integer(uhal::tests::usdiff(end,start)));
    
  }
}

void launch_threads(const size_t n_threads,const std::string& connection_file,const std::string& device_id) {
  std::vector<boost::thread *> jobs;

  for(size_t i=0; i!=n_threads; ++i) {
    jobs.push_back(new boost::thread(job,n_threads,connection_file,device_id));
  }
  
  boost::posix_time::time_duration timeout = boost::posix_time::seconds(TIMEOUT_S);
  for(size_t i=0; i!=n_threads; ++i) {
    jobs[i]->timed_join(timeout);
    delete jobs[i];
  }
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing(argc,argv);
  
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  std::cout << "STARTING TEST " << argv[0] 
	    << " (connection_file='" << connection_file <<"', "
	    << "device_id='" << device_id << "')..." << std::endl;

  uhal::log ( Notice() , "n_threads, dispatch_time [us]");

  for(size_t n_threads=1;n_threads <= MAX_THREADS;) {
    launch_threads(n_threads,connection_file,device_id);
    n_threads *=2;
  }
  
  return 0;
}
