#ifndef _uhal_HwInterface_hpp_
#define _uhal_HwInterface_hpp_

#include "uhal/AddressTable.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/Node.hpp"

namespace uhal {
  class HwInterface {
  public:
    HwInterface(const ClientInterface& client,const std::string& address_file)
      : client_(client),
	addressTable_(address_file)
    {
      
    }
    
    ClientInterface& getClient() {
      return client_;
    }

    void dispatch() {
      getClient().dispatch();
    }
    
    Node getNode(const std::string& id) {
      return Node(this,id);
    }

    std::vector<std::string> getNodes() {
      return addressTable_.getChildren("");
    }

    AddressTable& getAddressTable() {
      return addressTable_;
    }

  private:
    ClientInterface client_;
    AddressTable addressTable_;
  };
  
    
}

#endif 
