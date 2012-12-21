
#include "uhal/HwInterface.hpp"


namespace uhal
{

  HwInterface::HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const boost::shared_ptr< Node >& aNode ) :
    mClientInterface ( aClientInterface ),
    mNode ( aNode )
  {
    logging();
    claimNode ( *mNode );
  }


  HwInterface::HwInterface ( const HwInterface& otherHw ) :
    mClientInterface ( otherHw.mClientInterface ),
    mNode ( otherHw.mNode->clone() )
  {
    logging();
    claimNode ( *mNode );
  }


  HwInterface::~HwInterface()
  {
    logging();
  }

  void HwInterface::claimNode ( Node& aNode )
  {
    logging();
    aNode.mHw = this;

    for ( std::deque< Node* >::iterator lIt = aNode.mChildren.begin(); lIt != aNode.mChildren.end(); ++lIt )
    {
      claimNode ( **lIt );
    }
  }

  ClientInterface& HwInterface::getClient()
  {
    logging();
    return *mClientInterface;
  }

  // void HwInterface::ping()
  // {
  // try
  // {
  // mClientInterface->ping();
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }

  void HwInterface::dispatch ()
  {
    logging();
    mClientInterface->dispatch ();
  }


  const std::string& HwInterface::id() const
  {
    logging();
    return mClientInterface->id();
  }


  std::string HwInterface::uri() const
  {
    logging();
    return mClientInterface->uri();
  }


  void HwInterface::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
  {
    logging();
    mClientInterface->setTimeoutPeriod ( aTimeoutPeriod );
  }


  uint32_t HwInterface::getTimeoutPeriod()
  {
    logging();
    return mClientInterface->getTimeoutPeriod();
  }

  Node& HwInterface::getNode () const
  {
    logging();
    return *mNode;
  }


  Node& HwInterface::getNode ( const std::string& aId ) const
  {
    logging();
    return mNode->getNode ( aId );
  }

  std::vector<std::string> HwInterface::getNodes() const
  {
    logging();
    return mNode->getNodes();
  }

  std::vector<std::string> HwInterface::getNodes ( const std::string& aRegex ) const
  {
    logging();
    return mNode->getNodes ( aRegex );
  }

  // ValVector< uint32_t > HwInterface::readReservedAddressInfo ()
  // {
  // try
  // {
  // return mClientInterface->readReservedAddressInfo();
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );	// uhal::StdException lExc( aExc );
  // lExc.throwFrom( ThisLocation() );
  // }
  // }

}


