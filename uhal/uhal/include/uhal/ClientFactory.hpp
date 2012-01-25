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
    void add(const std::string& protocol) {
      std::map<std::string,CreatorInterface*>::const_iterator i(creators_.find(protocol));
      if (i != creators_.end())
	throw ProtocolAlreadyExist();
 		
      creators_[protocol] = new Creator<T>();
    }

    ClientInterface getClient(const std::string& id,const std::string& uri) {
      std::string protocol = getProtocol(uri);
      std::map<std::string,CreatorInterface*>::const_iterator i(creators_.find(protocol));
      if (i == creators_.end())
	throw ProtocolDoesNotExist();
      
      return i->second->create(id,uri);
      
    }

  private:
    ClientFactory() {}
    std::string getProtocol(const std::string& uri)  {
      return "ipbusudp";
    }

  private:
    class CreatorInterface {
    public:
      virtual ~CreatorInterface() {;}
      
      virtual ClientInterface create(const std::string& id,const std::string& uri) = 0;
    };

    template <class T>
    class Creator: public CreatorInterface {
    public:
      
      Creator() {};
      ClientInterface create(const std::string& id,const std::string& uri) {
	return T(id,uri);
      }
    };

  private:
    static ClientFactory* instance_;
    std::map<std::string,CreatorInterface*> creators_;
  };
}

#endif 
