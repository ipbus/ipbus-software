#ifndef _uhal_ClientInterface_hpp_
#define _uhal_ClientInterface_hpp_

#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"

#include <vector>
#include <iostream>

namespace uhal {
  class ClientInterface {
  public:
    ClientInterface(const std::string& id, const std::string& host,const int& port)
      :id_(id)
    {}
    std::string id() { return id_;}
    virtual bool ping() {return true;}
    virtual std::string url() {return "not implemented";}

    virtual void write(const uint32_t addr, const uint32_t val) {
      ValMem r(addr,val);
      tovalidate_.push_back(val);
      
    }

    virtual void write(const uint32_t& addr, const uint32_t& val, const uint32_t& mask) {
      ValMem r(addr,val);
      tovalidate_.push_back(val);
      
    }

    virtual void writeBlock(const uint32_t& addr, const std::vector<uint32_t>& val, const BlockReadWriteMode mode=NON_INCREMENTAL) {
      uint32_t acount(addr);
      for(std::vector<uint32_t>::const_iterator i(val.begin()); i!=val.end();++i,++acount) {
	ValMem v(acount,*i);
	tovalidate_.push_back(v);
	
      }
      
    }
    
    virtual ValMem read(const uint32_t& addr) {
      ValMem r(addr);
      tovalidate_.push_back(r);
      return r;
    }
    
    virtual ValMem read(const uint32_t& addr, const uint32_t& mask) {
      ValMem r(addr);
      tovalidate_.push_back(r);
      return r;
    }

    virtual std::vector<ValMem> readBlock(const uint32_t& addr, const uint32_t& size, const BlockReadWriteMode mode=INCREMENTAL) {
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
    virtual void dispatch() {
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
    std::vector<ValMem> tovalidate_;
    std::string id_;
  };
  
  
      
}

#endif 

