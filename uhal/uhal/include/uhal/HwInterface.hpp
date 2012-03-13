#ifndef _uhal_HwInterface_hpp_
#define _uhal_HwInterface_hpp_

#include "uhal/Node.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/Node.hpp"

namespace uhal
{
	class HwInterface
	{
		public:
			HwInterface ( const ClientInterface& aClientInterface , const Node& aAddressTable )
				: mClientInterface ( aClientInterface ),
				  mAddressTable ( aAddressTable )
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

			Node& getNode ( const std::string& aId )
			{
				//return Node ( this , aId );
			}

			std::vector<std::string> getNodes()
			{
				//return mAddressTable.getChildren ( "" );
			}

			// Node& getAddressTable()
			// {
				// return mAddressTable;
			// }

		private:
			ClientInterface mClientInterface;
			Node mAddressTable;

	};


}

#endif
