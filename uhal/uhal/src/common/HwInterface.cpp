
#include "uhal/HwInterface.hpp"


namespace uhal
{

	HwInterface::HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const Node& aNode )
		: mClientInterface ( aClientInterface ),
		  mNode ( aNode )
	{
	}

	boost::shared_ptr<ClientInterface> HwInterface::getClient()
	{
		return mClientInterface;
	}

	void HwInterface::dispatch ( defs::DispatchMode aMode )
	{
		mClientInterface->dispatch ( aMode );
	}

	Node& HwInterface::getNode ( const std::string& aId )
	{
		return mNode.getNode( aId );
	}

	std::vector<std::string> HwInterface::getNodes()
	{
		return mNode.getNodes();
	}
	
	std::vector<std::string> HwInterface::getNodes ( const boost::regex& aRegex )
	{
		return mNode.getNodes ( boost::regex( aRegex ) );	
	}

	std::vector<std::string> HwInterface::getNodes ( const char* aRegex )
	{
		return mNode.getNodes ( boost::regex( aRegex ) );
	}

	std::vector<std::string> HwInterface::getNodes ( const std::string& aRegex )
	{
		return mNode.getNodes ( boost::regex( aRegex ) );
	}	
	

	// Node& HwInterface::getAddressTable()
	// {
		// return mAddressTable;
	// }


}


