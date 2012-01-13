#ifndef _uhal_AddressTable_hpp_
#define _uhal_AddressTable_hpp_

#include "uhal/definitions.hpp"

#include "boost/utility.hpp"

#include <map>

#define uhal_NOMASK = 0xFFFFFFFF;

namespace uhal {
  class AddressTable {
  public:
    AddressTable(const std::string& filename)
      :filename_(filename) 
    {}

    uint32_t getAddress(const std::string& fullid) {
      return 0x32;
    }

    uint32_t getMask(const std::string& fullid) {
      return NOMASK;
    }

    NodePermission getPermission(const std::string& fullid) {
      return READWRITE;
    }
    
    std::vector<std::string> getChildren(const std::string& id) {
      std::vector<std::string> result;
      result.push_back(id + "." + "REG1");
      result.push_back(id + "." + "REG2");
      result.push_back(id + "." + "REG3");
      
      return result;
    }
    
  private:
    std::string filename_;
  };
}

#endif 
