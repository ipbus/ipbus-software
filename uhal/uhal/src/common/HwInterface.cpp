
#include "uhal/HwInterface.hpp"


namespace uhal
{

HwInterface::HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const boost::shared_ptr< const Node >& aNode ) try :
		mClientInterface ( aClientInterface ),
						 mNode ( new Node( *aNode ) )
	{
		claimNode ( *mNode );
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}


HwInterface::HwInterface ( const HwInterface& aHwInterface ) try :
		mClientInterface ( aHwInterface.mClientInterface ),
						 mNode ( new Node( *(aHwInterface.mNode) ) )
	{
		claimNode ( *mNode );
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}


HwInterface& HwInterface::operator= ( const HwInterface& aHwInterface ) 
	{
		try {
			mClientInterface = aHwInterface.mClientInterface;
			mNode = boost::shared_ptr< Node >( new Node( *(aHwInterface.mNode) ) );
			claimNode ( *mNode );
			
			return *this;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	
	
HwInterface::~HwInterface()
	{}
	

	void HwInterface::claimNode ( Node& aNode )
	{
		try
		{
			aNode.mHw = this;

			for ( std::hash_map< std::string , boost::shared_ptr<Node> >::iterator lIt = aNode.mChildren.begin() ; lIt != aNode.mChildren.end() ; ++lIt )
			{
				claimNode ( *(lIt->second) );
			}
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	boost::shared_ptr<ClientInterface> HwInterface::getClient()
	{
		try
		{
			return mClientInterface;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	void HwInterface::ping()
	{
		try
		{
			mClientInterface->ping();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	void HwInterface::dispatch ()
	{
		try
		{
			mClientInterface->dispatch ();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


	void HwInterface::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
	{
		mClientInterface->setTimeoutPeriod ( aTimeoutPeriod );
	}


	const uint32_t& HwInterface::getTimeoutPeriod()
	{
		return mClientInterface->getTimeoutPeriod();
	}



	Node& HwInterface::getNode ( const std::string& aId )
	{
		try
		{
			return mNode->getNode ( aId );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> HwInterface::getNodes()
	{
		try
		{
			return mNode->getNodes();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> HwInterface::getNodes ( const boost::regex& aRegex )
	{
		try
		{
			return mNode->getNodes ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> HwInterface::getNodes ( const char* aRegex )
	{
		try
		{
			return mNode->getNodes ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> HwInterface::getNodes ( const std::string& aRegex )
	{
		try
		{
			return mNode->getNodes ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< uint32_t > HwInterface::readReservedAddressInfo ()
	{
		try
		{
			return mClientInterface->readReservedAddressInfo();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	// Node& HwInterface::getAddressTable()
	// {
	// return mAddressTable;
	// }


}


