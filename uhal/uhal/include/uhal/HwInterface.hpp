#ifndef _uhal_HwInterface_hpp_
#define _uhal_HwInterface_hpp_

#include "uhal/Node.hpp"
#include "uhal/ClientInterface.hpp"


namespace uhal
{
	class HwInterface
	{
		public:
			HwInterface ( const ClientInterface& aClientInterface , const Node& aNode );

			ClientInterface& getClient();

			void dispatch ( defs::DispatchMode aMode = defs::NON_ATOMIC );

			Node& getNode ( const std::string& aId );

			std::vector<std::string> getNodes();

			// Node& getNode();

		private:
			ClientInterface mClientInterface;
			Node mNode;

	};


}

#endif
