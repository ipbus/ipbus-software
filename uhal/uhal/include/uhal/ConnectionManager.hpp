#ifndef _uhal_ConnectionManager_hpp_
#define _uhal_ConnectionManager_hpp_

#include "uhal/ClientInterface.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/ClientFactory.hpp"

#include "uhal/Utilities.hpp"



#include "boost/utility.hpp"
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>

#include "pugixml/pugixml.hpp"

#include <vector>
#include <iostream>





namespace uhal
{
	class DuplicatedUID: public std::exception {  };
	class ConnectionUIDDoesNotExist: public std::exception {  };

	
	class ConnectionManager: private boost::noncopyable
	{
		public:
			struct tConnectionDescriptor{
			
				tConnectionDescriptor( const pugi::xml_node& aNode , bool& aSuccess ){
					aSuccess=false;
					if ( ! uhal::utilities::GetXMLattribute<true>( aNode , "id" , id ) ) return;
					if ( ! uhal::utilities::GetXMLattribute<true>( aNode , "uri" , uri ) ) return;
					if ( ! uhal::utilities::GetXMLattribute<true>( aNode , "address_table" , address_table ) ) return;
					aSuccess=true;
				}			
				
				bool operator==( const tConnectionDescriptor& aConnectionDescriptor ) const{
					if( id != aConnectionDescriptor.id ) return false;
					if( uri != aConnectionDescriptor.uri ) return false;
					if( address_table != aConnectionDescriptor.address_table ) return false;
					return true;
				}
				
				std::string id;
				std::string uri;
				std::string address_table;		
			};
	
	
	
	
		public:
			//!Given a glob expression, parse all the files matching it (e.g. $BUILD/config/*.xml). If one parsing fails throw an exception and return filename and line number
			ConnectionManager ( const std::string& aFilenameExpr )
				:mFilenameExpr ( aFilenameExpr )
			{
				
				uhal::utilities::ParseSemicolonDelimitedUriList<true>( aFilenameExpr , mConnectionFiles );

				pugi::xml_document lXMLdocument;
			
				for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = mConnectionFiles.begin() ; lIt != mConnectionFiles.end() ; ++lIt ){
					if( lIt->first == "file" ){
						std::vector< boost::filesystem::path > lFilePaths;
						uhal::utilities::ShellExpandFilenameExpr<true>( lIt->second , &lFilePaths );
					
						for( std::vector< boost::filesystem::path >::iterator lIt2 = lFilePaths.begin() ; lIt2 != lFilePaths.end() ; ++ lIt2 ){
							pugi::xml_parse_result lLoadResult = lXMLdocument.load_file( lIt2->c_str() );
							if( !lLoadResult ){
								//Mark says to throw on this condition, I will leave it continuing for now...
								std::cout << "Failed to parse file " << *lIt2 << ". PugiXML returned the following description \"" << lLoadResult.description() << "\". Continuing for now but be aware!" << std::endl;
							}else{
								ParseConnectionsFile( lXMLdocument );
							}
						}
					
					}else if( lIt->first == "http" ){
						utilities::HttpResponseType lHttpResponse;
						uhal::utilities::HttpGet<true>( lIt->second , lHttpResponse );

						pugi::xml_parse_result lLoadResult = lXMLdocument.load_buffer( &(lHttpResponse.content[0]) , lHttpResponse.content.size() );
						if( !lLoadResult ){
							//Mark says to throw on this condition, I will leave it continuing for now...
							std::cout << "Failed to parse file " << (lIt->second) << ". PugiXML returned the following description \"" << lLoadResult.description() << "\". Continuing for now but be aware!" << std::endl;
						}else{
							ParseConnectionsFile( lXMLdocument );
						}
							
					}else{
					
					}

				}
			}
			
			
			
			/**
			 * Retrieves protocol, host, and port from the connection file to create the ClientInterface.
			 * Retrieves the address table file from the connection file to create the HwInterface.
			 */
			HwInterface getDevice ( const std::string& aId )
			{
				std::map< std::string, tConnectionDescriptor >::iterator lIt = mConnectionDescriptors.find( aId );
				
				if( lIt == mConnectionDescriptors.end() ){
					std::cout << aId << " does not exist in connection map" << std::endl;
					throw ConnectionUIDDoesNotExist();
				}
				
				ClientInterface lClientInterface = ClientFactory::getInstance().getClient ( lIt->second.id , lIt->second.uri );
				return HwInterface ( lClientInterface , lIt->second.address_table );
			}

			
			
			//Given a regex return the ids that match the
			std::vector<std::string> getDevices ( const boost::regex& aRegex )
			{
				std::vector<std::string> lDevices;

				for ( std::map< std::string, tConnectionDescriptor >::iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt ){
					boost::cmatch lMatch;
					if( boost::regex_match( lIt->first.c_str() , lMatch , aRegex ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
					{
						lDevices.push_back ( lIt->first );
					}
				}
				
				return lDevices;
			}

			std::vector<std::string> getDevices ( const char* aRegex )
			{
				return getDevices ( boost::regex( aRegex ) );
			}

			std::vector<std::string> getDevices ( const std::string& aRegex )
			{
				return getDevices ( boost::regex( aRegex ) );
			}	



		private:
			void ParseConnectionsFile( pugi::xml_document& aXmlDocument ){
				
				pugi::xpath_node_set lConnections = aXmlDocument.select_nodes("/connections/connection" );
				
				for (pugi::xpath_node_set::const_iterator lConnectionIt = lConnections.begin(); lConnectionIt != lConnections.end(); ++lConnectionIt )
				{
					bool lSuccess;
					tConnectionDescriptor lDescriptor( lConnectionIt->node() , lSuccess );
						
					if ( lSuccess ){
						std::pair< std::map< std::string, tConnectionDescriptor >::iterator , bool > lInsert = mConnectionDescriptors.insert( std::make_pair( lDescriptor.id , lDescriptor ) );
						if( !lInsert.second ){
							 tConnectionDescriptor& lExistingDescriptor( lInsert.first->second );
							if( lExistingDescriptor == lDescriptor ){
								std::cout << "Duplicate connection entry found:" << std::endl;
								std::cout << " > id = " << lDescriptor.id << std::endl;
								std::cout << " > uri = " << lDescriptor.uri << std::endl;
								std::cout << " > address_table = " << lDescriptor.address_table << std::endl;
								std::cout << "Continuing for now but be aware!" << std::endl;
							}else{
								std::cout << "Duplicate connection ID found but parameters do not match! Bailing!" << std::endl;
								throw DuplicatedUID();
							}
						}								
					}else{
						std::cout << "Construction of Connection Descriptor failed. Continuing for now but be aware!" << std::endl;
					}
						
				}
			}
			
			

		private:
			std::string mFilenameExpr;
			std::vector< std::pair<std::string, std::string> >  mConnectionFiles;
			std::map< std::string, tConnectionDescriptor >  mConnectionDescriptors;
						
	};


}

#endif

