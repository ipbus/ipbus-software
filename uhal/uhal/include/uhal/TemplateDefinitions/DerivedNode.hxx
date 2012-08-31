#include "uhal/NodeTreeBuilder.hpp"

namespace uhal
{

  template < typename DerivedType >
  Node* DerivedNode< DerivedType >::clone() const
  {
    try
    {
      return new DerivedType( static_cast<const DerivedType&>(*this) ); // call the copy ctor. 
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException( aExc ).throwFrom ( ThisLocation() );
    }
  }

  
  template< typename T >
  RegistrationHelper< T >::RegistrationHelper ( const std::string& aDerivedClassName )
  {
    NodeTreeBuilder::getInstance().add< T > ( aDerivedClassName );
  }  
}

