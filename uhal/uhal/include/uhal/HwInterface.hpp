#ifndef _uhal_HwInterface_hpp_
#define _uhal_HwInterface_hpp_

#include "uhal/Node.hpp"
#include "uhal/ClientInterface.hpp"

#include <boost/regex.hpp>


namespace uhal
{
	class HwInterface
	{
		public:
			HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const Node& aNode );

			boost::shared_ptr<ClientInterface> getClient();

			void dispatch ();

			Node& getNode ( const std::string& aId );

			std::vector<std::string> getNodes();

			std::vector<std::string> getNodes ( const boost::regex& aRegex );
			std::vector<std::string> getNodes ( const char* aRegex );
			std::vector<std::string> getNodes ( const std::string& aRegex );

			// Node& getNode();

			ValVector< uint32_t > readReservedAddressInfo ();

		private:
			void claimNode ( Node& aNode );

			boost::shared_ptr<ClientInterface> mClientInterface;
			Node mNode;

	};


}

#endif
