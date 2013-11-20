#ifndef _DummyDerivedNode_hpp_
#define _DummyDerivedNode_hpp_

#include "uhal/DerivedNode.hpp"

#include "uhal/log/log.hpp"
#include <iostream>

class DummyDerivedNode : public uhal::DerivedNode< DummyDerivedNode > {
public:
    DummyDerivedNode ( const Node& aNode ) : uhal::DerivedNode< DummyDerivedNode >(aNode) {}
    virtual ~DummyDerivedNode() {}

    void print() const {
      const boost::unordered_map<std::string, std::string>& lParameters = getParameters();
		boost::unordered_map<std::string, std::string>::const_iterator it;
		for ( it = lParameters.begin() ; it != lParameters.end() ; ++it ) 
			std::cout << it->first << ":" << it->second << std::endl;
	}

private:
   std::vector< std::pair<std::string, std::string> > mAttributes;
	
};

UHAL_REGISTER_DERIVED_NODE( DummyDerivedNode )

    
class DoubleDerivedNode : public uhal::DerivedNode< DoubleDerivedNode, DummyDerivedNode >
{
  public:
    DoubleDerivedNode ( const Node& aNode ) : uhal::DerivedNode< DoubleDerivedNode, DummyDerivedNode >(aNode) {}
    
    void print() const {
      log( uhal::Warning(), "Header");
        DummyDerivedNode::print();
      log( uhal::Warning(), "Footer");
	}
};

UHAL_REGISTER_DERIVED_NODE( DoubleDerivedNode )
#endif
