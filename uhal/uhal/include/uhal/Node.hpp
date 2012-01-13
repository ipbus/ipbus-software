#ifndef _uhal_Node_hpp_
#define _uhal_Node_hpp_

#include "uhal/definitions.hpp"

#include <exception>
#include <vector>
#include <string> 

namespace uhal {
  class HwInterface;
  class ValMem;

  class WriteAccessDenied: public std::exception {  };
  class ReadAccessDenied: public std::exception {  };

  class Node {
  public:
    Node(HwInterface* hw, const std::string& fullid);
    bool operator == (const Node& node) {
      return this->getAddress() == node.getAddress() && 
	this->getMask() == node.getMask() &&
	this->getPermission() == node.getPermission() &&
	this->getId() == node.getId();
    }


    /** 
     * Retrieve node with the relative id. If the node does not exist throws
     */
    Node getNode(const std::string& id);
    std::vector<std::string> getNodes();

    std::string getId() const{
      return fullid_;
    }

    uint32_t getAddress() const {
      return addr_;
    }

    uint32_t getMask() const {
      return mask_;
    }

    NodePermission getPermission() const {
      return permission_;
    }

    /**
     * Queues the corresponding operation. Id the permissions are insuficient or the node is not an end node, then it throws
     */
    void writeBlock(const std::vector<uint32_t>& vals,const BlockReadWriteMode mode=NON_INCREMENTAL);
    
    void write(const uint32_t val);
    
    std::vector<ValMem> readBlock(const uint32_t size, const BlockReadWriteMode mode=NON_INCREMENTAL);
    
    ValMem read();

  private:
    HwInterface* hw_;
    std::string fullid_;
    uint32_t addr_;
    uint32_t mask_;
    NodePermission permission_;
    
  };
    
}

#endif 
