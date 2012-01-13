#include "uhal/ClientImplementation.hpp"

#include "uhal/ClientFactory.hpp"

namespace uhal {
  
  ClientFactory* ClientFactory::instance_ = 0;

  ClientFactory& ClientFactory::getInstance() {
    if (instance_ == 0) {
      instance_ = new ClientFactory();
      
      instance_->add<uhal::IPBusUDPClient>("IpBus/UDP");

      instance_->add<uhal::ControlHubClient>("ControlHub/TCP");
      
      instance_->add<uhal::DummyClient>("Dummy");

    } 
    
    return *instance_;
    
  }
}
