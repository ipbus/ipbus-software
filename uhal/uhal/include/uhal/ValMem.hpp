#ifndef _uhal_ValMem_hpp_
#define _uhal_ValMem_hpp_

#include <boost/shared_ptr.hpp>

#include <vector>
#include <iostream>

namespace uhal {  
  class NonValidatedMemory: public std::exception {  };
  
  class ValMem {
  public:
    
    ValMem(const uint32_t& addr):
      valid_(new bool(false)),
      addr_(new uint32_t(addr)),
      value_(new uint32_t())
    { 
    };
    
    ValMem(const uint32_t& addr,const uint32_t& value):
      valid_(new bool(false)),
      addr_(new uint32_t(addr)),
      value_(new uint32_t(value))
    { 
    };

    bool getValid() {
      return *valid_;
    };
    
    void setValid(bool valid) {
      *valid_ = valid;
    };

    uint32_t getValue() {
      if (*valid_) 
	return *value_;
      else
	throw NonValidatedMemory();
    };

    void setValue(uint32_t value) {
      *value_ = value;
      *valid_ = true;
    };

    operator uint32_t() {
      return getValue();
    };

    uint32_t getAddress() {
      if (*valid_) 
	return *value_;
      else
	throw NonValidatedMemory();
    };

    void setAddress(uint32_t addr) {
      *addr_ = addr;
    };
    
    //!Just for cout
    friend std::ostream &operator<<(std::ostream &out, ValMem v) {
      out << v.getValue();
      return out;
    };
    
  private:
    ValMem();

  private:
    boost::shared_ptr<bool> valid_;
    boost::shared_ptr<uint32_t> addr_;
    boost::shared_ptr<uint32_t> value_;

  };
  
 
      
}

#endif 

