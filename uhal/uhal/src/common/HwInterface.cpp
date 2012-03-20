
#include "uhal/HwInterface.hpp"


namespace uhal
{

	HwInterface::HwInterface ( const ClientInterface& aClientInterface , const Node& aNode )
		: mClientInterface ( aClientInterface ),
		  mNode ( aNode )
	{
	}

	ClientInterface& HwInterface::getClient()
	{
		return mClientInterface;
	}

	void HwInterface::dispatch ( defs::DispatchMode aMode )
	{
		getClient().dispatch ( aMode );
	}

	Node& HwInterface::getNode ( const std::string& aId )
	{
		//return Node ( this , aId );
	}

	std::vector<std::string> HwInterface::getNodes()
	{
		//return mAddressTable.getChildren ( "" );
	}

	// Node& HwInterface::getAddressTable()
	// {
		// return mAddressTable;
	// }


}


