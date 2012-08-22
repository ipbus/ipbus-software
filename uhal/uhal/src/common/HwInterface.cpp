
#include "uhal/HwInterface.hpp"


namespace uhal
{

HwInterface::HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const boost::shared_ptr< Node >& aNode ) try :
		mClientInterface ( aClientInterface ),
			mNode ( aNode )
	{
		claimNode ( *mNode );
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

HwInterface::HwInterface ( const HwInterface& otherHw ) try :
		mClientInterface ( otherHw.mClientInterface ),
			mNode( otherHw.mNode->clone() )
	{
		claimNode ( *mNode );
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	} 

	HwInterface::~HwInterface()
	{}

	void HwInterface::claimNode ( Node& aNode )
	{
		try
		{
			aNode.mHw = this;

			for ( std::deque< Node* >::iterator lIt = aNode.mChildren.begin(); lIt != aNode.mChildren.end(); ++lIt )
			{
				claimNode ( **lIt );
			}
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

	ClientInterface& HwInterface::getClient()
	{
		try
		{
			return *mClientInterface;
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

	// void HwInterface::ping()
	// {
	// try
	// {
	// mClientInterface->ping();
	// }
	// catch ( uhal::exception& aExc )
	// {
	// aExc.rethrowFrom ( ThisLocation() );
	// }
	// catch ( const std::exception& aExc )
	// {
	// StdException ( aExc ).throwFrom ( ThisLocation() );
	// }
	// }

	void HwInterface::dispatch ()
	{
		try
		{
			mClientInterface->dispatch ();
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


	const std::string& HwInterface::id()
	{
		try
		{
			return mClientInterface->id();
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


	std::string HwInterface::uri()
	{
		try
		{
			return mClientInterface->uri();
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


	void HwInterface::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
	{
		try
		{
			mClientInterface->setTimeoutPeriod ( aTimeoutPeriod );
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


	uint32_t HwInterface::getTimeoutPeriod()
	{
		try
		{
			return mClientInterface->getTimeoutPeriod();
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

	Node& HwInterface::getNode ()
	{
		try
		{
			return *mNode;
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


	Node& HwInterface::getNode ( const std::string& aId )
	{
		try
		{
			return mNode->getNode ( aId );
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

	std::vector<std::string> HwInterface::getNodes()
	{
		try
		{
			return mNode->getNodes();
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

	std::vector<std::string> HwInterface::getNodes ( const std::string& aRegex )
	{
		try
		{
			return mNode->getNodes ( aRegex );
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


