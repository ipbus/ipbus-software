#ifndef _uhal_HwInterface_hpp_
#define _uhal_HwInterface_hpp_

#include "uhal/AddressTable.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/Node.hpp"

namespace uhal
{
	class HwInterface
	{
		public:
			HwInterface ( const ClientInterface& aClientInterface , const std::string& aFileName )
				: mClientInterface ( aClientInterface ),
				  mAddressTable ( aFileName )
			{
			}

			ClientInterface& getClient()
			{
				return mClientInterface;
			}

			void dispatch ( defs::DispatchMode aMode = defs::NON_ATOMIC )
			{
				getClient().dispatch ( aMode );
			}

			Node getNode ( const std::string& aId )
			{
				return Node ( this , aId );
			}

			std::vector<std::string> getNodes()
			{
				return mAddressTable.getChildren ( "" );
			}

			AddressTable& getAddressTable()
			{
				return mAddressTable;
			}

		private:
			ClientInterface mClientInterface;
			AddressTable mAddressTable;
	};


}

#endif
