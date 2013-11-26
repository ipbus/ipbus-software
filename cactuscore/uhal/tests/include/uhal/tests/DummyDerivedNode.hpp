#ifndef _DummyDerivedNode_hpp_
#define _DummyDerivedNode_hpp_

#include "uhal/DerivedNode.hpp"

#include "uhal/log/log.hpp"
#include <iostream>

class ClassLvl1Node : public uhal::Node {
  UHAL_DERIVEDNODE(ClassLvl1Node);
  public:

  ClassLvl1Node(const Node& aNode) : uhal::Node(aNode) {
  }

  virtual ~ClassLvl1Node() {
  }

  void printParameters() const {
    const boost::unordered_map<std::string, std::string>& lParameters = getParameters();
    boost::unordered_map<std::string, std::string>::const_iterator it;
    uint32_t k=0;
    for (it = lParameters.begin(); it != lParameters.end(); ++it, ++k)
      std::cout << getClassName() << " par[" << k << "]: key=" << it->first << ", val=" << it->second << std::endl;

    // print the list of childs as well?
  }

};

UHAL_REGISTER_DERIVED_NODE(ClassLvl1Node)

/**
 * Class further derived from Level1
 */

class ClassLvl2Node : public ClassLvl1Node {
  UHAL_DERIVEDNODE(ClassLvl2Node)
  public:
    ClassLvl2Node(const Node& aNode) : ClassLvl1Node(aNode) {
    }

    void printParameters() const {
      log(uhal::Warning(), "This is ", getClassName());
      ClassLvl1Node::printParameters();
    }
};

UHAL_REGISTER_DERIVED_NODE(ClassLvl2Node)
#endif
