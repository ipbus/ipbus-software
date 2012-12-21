#include "uhal/NodeTreeBuilder.hpp"

namespace uhal
{

  template < typename DerivedType >
  Node* DerivedNode< DerivedType >::clone() const
  {
    logging();
    return new DerivedType ( static_cast<const DerivedType&> ( *this ) ); // call the copy ctor.
  }


  template< typename T >
  RegistrationHelper< T >::RegistrationHelper ( const std::string& aDerivedClassName )
  {
    logging();
    NodeTreeBuilder::getInstance().add< T > ( aDerivedClassName );
  }
}

