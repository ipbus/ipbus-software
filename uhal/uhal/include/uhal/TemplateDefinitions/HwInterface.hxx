

namespace uhal
{

  template< typename T>
  T& HwInterface::getNode ( const std::string& aId ) const
  {
    logging();
    return mNode->getNode< T > ( aId );
  }

}

