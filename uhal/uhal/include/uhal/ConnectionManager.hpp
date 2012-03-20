#ifndef _uhal_ConnectionManager_hpp_
#define _uhal_ConnectionManager_hpp_

#include "uhal/HwInterface.hpp"

#include <boost/filesystem.hpp>
#include <boost/regex.hpp>

#include "pugixml/pugixml.hpp"

#include <vector>
#include <set>
#include <map>

namespace uhal
{
	class DuplicatedUID: public std::exception {  };
	class ConnectionUIDDoesNotExist: public std::exception {  };

	
	class ConnectionManager: private boost::noncopyable
	{
		public:
			struct tConnectionDescriptor{
				tConnectionDescriptor( const pugi::xml_node& aNode , bool& aSuccess );
				bool operator==( const tConnectionDescriptor& aConnectionDescriptor ) const;
				
				std::string id;
				std::string uri;
				std::string address_table;		
			};
	
	
	
	
		public:
			//!Given a glob expression, parse all the files matching it (e.g. $BUILD/config/*.xml). If one parsing fails throw an exception and return filename and line number
			ConnectionManager ( const std::string& aFilenameExpr );

			virtual ~ConnectionManager ();
			
			/**
			 * Retrieves protocol, host, and port from the connection file to create the ClientInterface.
			 * Retrieves the address table file from the connection file to create the HwInterface.
			 */
			HwInterface getDevice ( const std::string& aId );
			
			//Given a regex return the ids that match the
			std::vector<std::string> getDevices ( const boost::regex& aRegex );
			std::vector<std::string> getDevices ( const char* aRegex );
			std::vector<std::string> getDevices ( const std::string& aRegex );

		private:
			void CallBack( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile );

		private:
			std::vector< std::pair<std::string, std::string> >  mConnectionFiles;
			std::map< std::string, tConnectionDescriptor >  mConnectionDescriptors;
			std::set< std::string > mPreviouslyOpenedFiles;
						
	};


}

#endif

