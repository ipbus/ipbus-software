#ifndef _uhal_ClientFactory_hpp_
#define _uhal_ClientFactory_hpp_

#include "uhal/ClientInterface.hpp"

#include "boost/utility.hpp"

#include <map>

namespace uhal {
  class ProtocolAlreadyExist: public std::exception {  };
  class ProtocolDoesNotExist: public std::exception {  };
  
  class ClientFactory: private boost::noncopyable {
  public:
    static ClientFactory& getInstance();

    template <class T> 
    void add(const std::string& name) {
      std::map<std::string,CreatorInterface*>::const_iterator i(creators_.find(name));
      if (i != creators_.end())
	throw ProtocolAlreadyExist();
 		
      creators_[name] = new Creator<T>();
    }

    ClientInterface getClient(const std::string& protocol, const std::string& id,const std::string& host,const int& port) {
      std::map<std::string,CreatorInterface*>::const_iterator i(creators_.find(protocol));
      if (i == creators_.end())
	throw ProtocolDoesNotExist();
      
      return i->second->create(id,host,port);
      
    }

  private:
    ClientFactory() {}

  private:
    class CreatorInterface {
    public:
      virtual ~CreatorInterface() {;}
      
      virtual ClientInterface create(const std::string& id,const std::string& host,const int& port) = 0;
    };

    template <class T>
    class Creator: public CreatorInterface {
    public:
      
      Creator() {};
      ClientInterface create(const std::string& id,const std::string& host,const int& port) {
	return T(id,host,port);
      }
    };

  private:
    static ClientFactory* instance_;
    std::map<std::string,CreatorInterface*> creators_;
  };
}

#endif 
