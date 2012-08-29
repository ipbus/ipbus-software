

namespace uhal
{

  template< typename T>
  T& HwInterface::getNode ( const std::string& aId )
  {
    try
    {
      return mNode->getNode< T > ( aId );
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

}

