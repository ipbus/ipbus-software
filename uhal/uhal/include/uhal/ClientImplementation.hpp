#ifndef _uhal_ClientImplementation_hpp_
#define _uhal_ClientImplementation_hpp_

#include "uhal/ClientInterface.hpp"

namespace uhal {
  class IPBusUDPClient: public ClientInterface {
  public:
    IPBusUDPClient(const std::string& id,const std::string& uri)
      :ClientInterface(id,uri) 
    {};
  };

  class IPBusTCPClient: public ClientInterface {
  public:
    IPBusTCPClient(const std::string& id,const std::string& uri)
      :ClientInterface(id,uri) 
    {};
  };

  class ControlHubClient: public ClientInterface {
  public:
    ControlHubClient(const std::string& id,const std::string& uri)
      :ClientInterface(id,uri) 
    {};
  };

  class DummyClient: public ClientInterface {
  public:
    DummyClient(const std::string& id,const std::string& uri)
      :ClientInterface(id,uri) 
    {};
  };


    
}

#endif 
