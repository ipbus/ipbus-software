#ifndef _uhal_AddressTableBuilder_hpp_
#define _uhal_AddressTableBuilder_hpp_

#include "uhal/definitions.hpp"
#include "uhal/Node.hpp"

#include <boost/utility.hpp>
#include <boost/filesystem.hpp>

#include "pugixml/pugixml.hpp"

#include <map>

namespace uhal
{
	class IncorrectAddressTableFileCount: public std::exception {  };
	class FailedToParseAddressTableFile: public std::exception {  };


	class AddressTableBuilder: private boost::noncopyable
	{
		private:
			AddressTableBuilder () {}
			
			virtual ~AddressTableBuilder () {}
			
			
		public:
			static AddressTableBuilder& getInstance();
				
			Node getAddressTable( const std::string& aFilenameExpr );
			
			//bool BuildFromAddressFile( const pugi::xml_document& aXmlDocument ){}

			//bool BuildFromAddressFile( const std::string&  ){}
			

			Node CallBack( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile );



			

		private:
			static AddressTableBuilder* mInstance;
			std::map< std::string , Node > mAddressTables;
	};
}

#endif
