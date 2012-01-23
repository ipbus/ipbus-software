#ifndef _uhal_ClientInterface_hpp_
#define _uhal_ClientInterface_hpp_

#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"

#include <vector>
#include <iostream>

namespace uhal {
  class AtomicTransactionSize: public std::exception {};

  class ClientInterface {
  public:
    ClientInterface(const std::string& id, const std::string& location)
      :id_(id)
    {}
    virtual ~ClientInterface() {}

    std::string id() { return id_;}
    virtual bool ping() {return true;}
    virtual std::string url() {return "not implemented";}

    virtual void write(const uint32_t addr, const uint32_t val) {
      ValMem r(val);
      tovalidate_.push_back(val);
      
    }

    virtual void write(const uint32_t& addr, const uint32_t& val, const uint32_t& mask) {
      uint32_t v = (val << trailing_right_bits(mask)) && mask;
      
      ValMem r(v);
      tovalidate_.push_back(val);
      
    }

    virtual void writeBlock(const uint32_t& addr, const std::vector<uint32_t>& val, const defs::BlockReadWriteMode mode=defs::INCREMENTAL) {
      for(std::vector<uint32_t>::const_iterator i(val.begin()); i!=val.end();++i) {
	ValMem v(*i);
	tovalidate_.push_back(v);
	
      }
      
    }
    
    virtual ValMem read(const uint32_t& addr) {
      ValMem r(rand());
      tovalidate_.push_back(r);
      return r;
    }
    
    virtual ValMem read(const uint32_t& addr, const uint32_t& mask) {
      uint32_t val = rand();
      val = (val & mask) >> trailing_right_bits(mask);
      ValMem r(val);
      tovalidate_.push_back(r);
      return r;
    }

    virtual std::vector<ValMem> readBlock(const uint32_t& addr, const uint32_t& size, const defs::BlockReadWriteMode mode=defs::INCREMENTAL) {
      std::vector<ValMem> r;
      uint32_t acount(addr);
      for(uint32_t i(0); i!= size; ++i,++acount) {
	ValMem v(acount);
	r.push_back(v);
	tovalidate_.push_back(v);
	  
      }
      
      return r;
    }
    
    //validation has to be moved to the descendants
    virtual void dispatch(defs::DispatchMode mode = defs::NON_ATOMIC) {
      if (mode == defs::ATOMIC && tovalidate_.size() > MAX_REQUEST_PER_PAQUET)
	throw AtomicTransactionSize();

      try{
	for(std::vector<ValMem>::iterator i(tovalidate_.begin()); i!=tovalidate_.end();++i)
	  i->setValid(true);
	tovalidate_.clear();
      } catch(std::exception& e) {
	//dispatch failure, do we allow partial failure?
	tovalidate_.clear();
	throw;
      }
    }
  private:
    unsigned int trailing_right_bits(uint32_t v) {
      unsigned int c = sizeof(v)*8; // c will be the number of zero bits on the right
      v &= -signed(v);
      if (v) c--;
      if (v & 0x0000FFFF) c -= 16;
      if (v & 0x00FF00FF) c -= 8;
      if (v & 0x0F0F0F0F) c -= 4;
      if (v & 0x33333333) c -= 2;
      if (v & 0x55555555) c -= 1;

      return c;
    }
  private:
    static const size_t MAX_REQUEST_PER_PAQUET = 1500/8/2;
    std::vector<ValMem> tovalidate_;
    std::string id_;
  };
  
  
      
}

#endif 

