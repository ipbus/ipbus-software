

namespace uhal
{

  template< typename T>
  T& Node::getNode ( const std::string& aId ) const
  {
    logging();

    try
    {
      return dynamic_cast< T& > ( getNode ( aId ) );
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Invalid cast of Node " , Quote ( getNode ( aId ).getId() ) , " from type ", Quote ( Type ( *this ) ), " to " ,  Quote ( Type<T>() ) );
      throw BadNodeCast();
    }
  }

}

