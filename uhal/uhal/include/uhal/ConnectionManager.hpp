#ifndef _uhal_ConnectionManager_hpp_
#define _uhal_ConnectionManager_hpp_

#include "uhal/ClientInterface.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/ClientFactory.hpp"

#include "boost/utility.hpp"

#include <vector>
#include <iostream>

namespace uhal {
  
  class ConnectionManager: private boost::noncopyable {
  public:
    //!Given a glob expression, parse all the files matching it (e.g. $BUILD/config/*.xml). If one parsing fails throw an exception and return filename and line number
    ConnectionManager(const std::string& filename_expr)
      :filename_expr_(filename_expr) 
    {}
    
    /** 
     * Retrieves protocol, host, and port from the connection file to create the ClientInterface.
     * Retrieves the address table file from the connection file to create the HwInterface.
     */
    HwInterface getDevice(const std::string& id) {
      ClientInterface c = ClientFactory::getInstance().getClient("IpBus/UDP",id,"hcal.cern.ch",1234);
      return HwInterface(c,"addr/addresses.xml");
    }
    
    
    //Given a regex return the ids that match the
    std::vector<std::string> getDevices(const std::string&) {
      std::vector<std::string> r;
      
      r.push_back("hcal.crate1.slot1");
      r.push_back("hcal.crate1.slot2");
      r.push_back("gt.crate1.psb");
      r.push_back("gt-spare.crate1.tcs");
      
      return r;
    }
  
  private:
    std::string filename_expr_;
    
  };
  
      
}

#endif 

