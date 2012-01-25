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
    ClientInterface(const std::string& id, const std::string& uri)
      :id_(id)
    {}
    virtual ~ClientInterface() {}

    std::string id() { return id_;}
    virtual bool ping() {return true;}
    virtual std::string url() {return "not implemented";}

    virtual void write(const uint32_t addr, const uint32_t& value) {
      ValWord v(value);
      valwords_.push_back(v);
      
    }

    virtual void write(const uint32_t& addr, const uint32_t& value, const uint32_t& mask) {
      
      ValWord val((val << trailing_right_bits(mask)) && mask);
      valwords_.push_back(val);
      
    }

    virtual void writeBlock(const uint32_t& addr, const std::vector<uint32_t>& values, const defs::BlockReadWriteMode mode=defs::INCREMENTAL) {
      ValBlock v;
      v.assign(values.begin(),values.end());
      valblocks_.push_back(v);
    }
    
    virtual ValWord read(const uint32_t& addr) {
      ValWord v(rand());
      valwords_.push_back(v);
      return v;
    }
    
    virtual ValWord read(const uint32_t& addr, const uint32_t& mask) {
      uint32_t val = rand();
      val = (val & mask) >> trailing_right_bits(mask);
      ValWord r(val);
      valwords_.push_back(r);
      return r;
    }

    virtual ValBlock readBlock(const uint32_t& addr, const uint32_t& size, const defs::BlockReadWriteMode mode=defs::INCREMENTAL) {
      std::vector<uint32_t> r;
      for(uint32_t i(0); i!= size; ++i) {
	r.push_back(rand());
      }
      ValBlock v(r);
      valblocks_.push_back(v);
	  
      
      return v;
    }
    
    //validation has to be moved to the descendants
    virtual void dispatch(defs::DispatchMode mode = defs::NON_ATOMIC) {
      if (mode == defs::ATOMIC && 
	  (valblocks_.size()+valwords_.size()) > MAX_REQUEST_PER_PAQUET)
	throw AtomicTransactionSize();

      try{
	for(std::vector<ValWord>::iterator i(valwords_.begin()); i!=valwords_.end();++i)
	  i->valid(true);
	
	valwords_.clear();
	
	for(std::vector<ValBlock>::iterator i(valblocks_.begin()); i!=valblocks_.end();++i)
	  i->valid(true);
	
	valblocks_.clear();

      } catch(std::exception& e) {
	valwords_.clear();
	valblocks_.clear();
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
    std::vector<ValWord> valwords_;
    std::vector<ValBlock> valblocks_;
    std::string id_;
  };
  
  
      
}

#endif 

