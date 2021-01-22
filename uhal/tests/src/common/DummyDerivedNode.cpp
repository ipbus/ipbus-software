#include "uhal/tests/DummyDerivedNode.hpp"

namespace uhal {
  namespace tests {
    UHAL_REGISTER_DERIVED_NODE(DummyParentNode)
    UHAL_REGISTER_DERIVED_NODE(DummyChildNode)

    DummyParentNode::DummyParentNode(const uhal::Node& aNode) :
      uhal::Node(aNode) {
    }

    DummyParentNode::~DummyParentNode() {
    }

    void
    DummyParentNode::printParameters() const {

      std::stringstream ss;
#ifdef __GNUG__
      // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
      int lStatus(0);
      static std::size_t lSize(1024);
      static char* lDemangled = new char[lSize];
      ss << (abi::__cxa_demangle(typeid ( *this).name(), lDemangled, &lSize, &lStatus));
#else
      ss << (typeid ( T).name());
#endif

      std::cout << "This is " << ss.str() << std::endl;
      uint32_t k = 0;
      for (const auto& lParam: getParameters()) {
        std::cout << ss.str() << " par[" << k << "]: key=" << lParam.first << ", val=" << lParam.second << std::endl;
        k++;
      }

      // print the list of childs as well?
    }

    DummyChildNode::DummyChildNode(const uhal::Node& aNode) :
      uhal::tests::DummyParentNode(aNode) {
    }

    DummyChildNode::~DummyChildNode() {
    }

  }
}

