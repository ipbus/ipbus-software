#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/AddressTable.hpp"

namespace uhal {
  Node::Node(HwInterface* hw, const std::string& fullid)
    :hw_(hw),
     fullid_(fullid),
     addr_(hw->getAddressTable().getAddress(fullid)),
     mask_(hw->getAddressTable().getMask(fullid)),
     permission_(hw->getAddressTable().getPermission(fullid))
  {}
    
  Node Node::getNode(const std::string& id) {
    return Node(hw_,fullid_ + "." + id);
  }

  std::vector<std::string> Node::getNodes() {
    return hw_->getAddressTable().getChildren(fullid_);
  }
    
  void Node::writeBlock(const std::vector<uint32_t>& vals,defs::BlockReadWriteMode mode) {
    if (permission_ & defs::WRITE)
      hw_->getClient().writeBlock(addr_,vals,mode);
    else
      throw WriteAccessDenied();
  }
    
  void Node::write(const uint32_t& val) {
    if (permission_ & defs::WRITE) {
      if (mask_ == defs::NOMASK)
	hw_->getClient().write(addr_,val);
      else
	hw_->getClient().write(addr_,val,mask_);
    } else
      throw WriteAccessDenied();
    
  }
    
  ValBlock Node::readBlock(const uint32_t& size, defs::BlockReadWriteMode mode) {
    if (permission_ & defs::READ) 
      return hw_->getClient().readBlock(size,mode);
    else
      throw ReadAccessDenied();
  }
    
  ValWord Node::read() {
    if (permission_ & defs::READ) {
      if (mask_ == defs::NOMASK)
	return hw_->getClient().read(addr_);
      else
	return hw_->getClient().read(addr_,mask_);
    } else
      throw ReadAccessDenied();
  }

}
