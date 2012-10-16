/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_DerivedNode_hpp_
#define _uhal_DerivedNode_hpp_

#include "uhal/Node.hpp"

#define REGISTER( classname ) uhal::RegistrationHelper< classname > classname##RegistrationHelper( #classname );

namespace uhal
{

  template < typename DerivedType >
  class DerivedNode : public Node
  {
    protected:

      /**
        CRTP function to produce a new copy of the current Node
        @return a new copy of the current Node
      */
      virtual Node* clone() const;

  };

  template< typename T >
  struct RegistrationHelper
  {
    RegistrationHelper ( const std::string& aDerivedClassName );
  };

}

#include "uhal/TemplateDefinitions/DerivedNode.hxx"

#endif
