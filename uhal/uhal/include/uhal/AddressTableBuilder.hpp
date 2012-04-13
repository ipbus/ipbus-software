#ifndef _uhal_AddressTableBuilder_hpp_
#define _uhal_AddressTableBuilder_hpp_

#include "uhal/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/Node.hpp"

#include <boost/utility.hpp>
#include <boost/filesystem.hpp>

#include "pugixml/pugixml.hpp"

#include <map>

namespace uhal
{
	class IncorrectAddressTableFileCount: public uhal::exception {  };
	class FailedToOpenAddressTableFile: public uhal::exception {  };


	class AddressTableBuilder: private boost::noncopyable
	{
		private:
			AddressTableBuilder () {}

			virtual ~AddressTableBuilder () {}


		public:
			static AddressTableBuilder& getInstance();

			Node getAddressTable ( const std::string& aFilenameExpr );

			void CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , std::vector< Node >& aAddressTable );

		private:
			static AddressTableBuilder* mInstance;
			std::hash_map< std::string , Node > mNodes;

	};
}

#endif
