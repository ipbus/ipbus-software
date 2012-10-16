

namespace uhal
{

  template< typename T>
  T& Node::getNode ( const std::string& aId ) const
  {
    try
    {
      return dynamic_cast< T& > ( getNode ( aId ) );
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Invalid cast of Node " , Quote ( getNode ( aId ).getId() ) , " from type ", Quote ( Type ( *this ) ), " to " ,  Quote ( Type<T>() ) );
      BadNodeCast().throwFrom ( ThisLocation() );
    }
  }

}

