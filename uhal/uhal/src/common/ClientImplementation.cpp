#include "uhal/ClientImplementation.hpp"

#include "uhal/ClientFactory.hpp"

namespace uhal {
  
  ClientFactory* ClientFactory::instance_ = 0;

  ClientFactory& ClientFactory::getInstance() {
    if (instance_ == 0) {
      instance_ = new ClientFactory();
      
      instance_->add<uhal::IPBusUDPClient>("ipbusudp");

      instance_->add<uhal::IPBusTCPClient>("ipbustcp");

      instance_->add<uhal::ControlHubClient>("controlhuptcp");
      
      instance_->add<uhal::DummyClient>("dummy");


    } 
    
    return *instance_;
    
  }
}
