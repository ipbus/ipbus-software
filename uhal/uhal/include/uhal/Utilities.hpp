#ifndef _uhal_Utilities_hpp_
#define _uhal_Utilities_hpp_

#include <vector>
#include <string>
#include <iostream>

#include <wordexp.h>

#include <boost/filesystem.hpp>
#include <boost/asio.hpp>
#include <boost/spirit/include/qi.hpp>

#include "uhal/BoostSpiritGrammars.hpp"
#include "uhal/UtilityTypes.hpp"

#include "pugixml/pugixml.hpp"

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{

		template < bool DebugInfo >
		void ParseSemicolonDelimitedUriList( const std::string& aSemicolonDelimitedUriList , std::vector< std::pair<std::string, std::string> >& aUriList )
		{
		
			SemicolonDelimitedUriListGrammar lGrammar;
			boost::spirit::qi::phrase_parse( aSemicolonDelimitedUriList.begin() , aSemicolonDelimitedUriList.end() , lGrammar , boost::spirit::ascii::space , aUriList );
			
			if( DebugInfo ){
				std::cout << "Parsed \"" << aSemicolonDelimitedUriList << "\" to:" << std::endl;
				for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = aUriList.begin() ; lIt != aUriList.end() ; ++lIt ){
					std::cout << " > [" << lIt->first << "] \"" << lIt->second << "\"" << std::endl;
				}
			}
			
		}
	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


			
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{

	template < bool DebugInfo >
		void ShellExpandFilenameExpr( const char* aFilenameExpr , std::vector< boost::filesystem::path > * aFiles = NULL , std::vector< boost::filesystem::path > * aDirectories = NULL )
		{
			//struct which will store the shell expansions of the expression
			wordexp_t lShellExpansion;

			wordexp( aFilenameExpr , &lShellExpansion , 0 );

			for ( std::size_t i = 0 ; i != lShellExpansion.we_wordc ; i++ ){			
					boost::filesystem::path lPath( lShellExpansion.we_wordv[i] );
					if ( boost::filesystem::exists( lPath ) ){
						if( aFiles ){
							if ( boost::filesystem::is_regular_file( lPath ) ){
								aFiles->push_back(  boost::filesystem::absolute(lPath) );
							}
						}
						if( aDirectories ){
							if ( boost::filesystem::is_directory( lPath ) ){
								aDirectories->push_back(  boost::filesystem::absolute(lPath) );
							}
						}
					}			
				}			

			if( DebugInfo ){
				if( aFiles || aDirectories ) std::cout << "Shell expansion of \"" << aFilenameExpr << "\" returned:" << std::endl;
				if( aFiles ){
					for( std::vector< boost::filesystem::path >::iterator lIt = aFiles->begin() ; lIt !=  aFiles->end() ; ++lIt ){
						std::cout << " > [file] " << *lIt << std::endl;				
					}
					
					if ( ! aFiles->size() ){
						std::cout << " > No matching files." << std::endl;
					}
				}
				if( aDirectories ){
					for( std::vector< boost::filesystem::path >::iterator lIt = aDirectories->begin() ; lIt !=  aDirectories->end() ; ++lIt ){
						std::cout << " > [directory] " << *lIt << std::endl;				
					}
						
					if ( ! aDirectories->size() ){
						std::cout << " > No matching files." << std::endl;
					}
				}					
			}
				
			wordfree(&lShellExpansion);		
		}
		

		template < bool DebugInfo >
		void ShellExpandFilenameExpr( const std::string& aFilenameExpr , std::vector< boost::filesystem::path > * aFiles , std::vector< boost::filesystem::path > * aDirectories = NULL )
		{
			ShellExpandFilenameExpr< DebugInfo >( aFilenameExpr.c_str() , aFiles , aDirectories );
		}
		
	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

		
		
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{
	
		template < bool DebugInfo >
		bool HttpGet( const std::string& aURL , HttpResponseType& aResponse ){
			if( DebugInfo ) std::cout << "Retrieving URL http://" << aURL << std::endl;
		
			URLGrammar lGrammar;
			std::pair<std::string, std::string> lURLPair;
			boost::spirit::qi::phrase_parse( aURL.begin() , aURL.end() , lGrammar , boost::spirit::ascii::space , lURLPair );
								
			// The IO service everything will go through
			boost::asio::io_service io_service;
			
			// Get a list of endpoints corresponding to the server name.
			boost::asio::ip::tcp::resolver resolver( io_service );
			boost::asio::ip::tcp::resolver::query query( lURLPair.first , "http" );
			boost::asio::ip::tcp::resolver::iterator endpoint_iterator = resolver.resolve( query );
			boost::asio::ip::tcp::resolver::iterator end;

			// Try each endpoint until we successfully establish a connection.
			boost::asio::ip::tcp::socket socket(io_service);
			boost::system::error_code error = boost::asio::error::host_not_found;
			try{
				while (error && endpoint_iterator != end)
				{
					socket.close();
					socket.connect(*endpoint_iterator++, error);
				}
				if (error) throw boost::system::system_error(error);
			}catch( std::exception& aExc ){
				std::cout << "Exception: " << aExc.what() << std::endl;
				return false;
			}					
			
			// Form the request. We specify the "Connection: close" header so that the server will close the socket after transmitting the response. This will allow us to treat all data up until the EOF as the content.
			boost::asio::streambuf request;
			std::ostream request_stream(&request);
			request_stream << "GET /" << lURLPair.second << " HTTP/1.0\r\n";
			request_stream << "Host: " << lURLPair.first << "\r\n";
			request_stream << "Accept: */*\r\n";
			request_stream << "Connection: close\r\n\r\n";
			
			// Send the request...
			try{
				boost::asio::write( socket , request , error );
				if (error) throw boost::system::system_error(error);
			}catch( std::exception& aExc ){
				std::cout << "Exception: " << aExc.what() << std::endl;
				return false;
			}	
			
			
			// ... and get the reply. First we need a buffer to write the reply in to...
			static const int mDefaultBufferSize( 65536 );			
			typedef std::vector<uint8_t> BufferType;
			typedef BufferType::iterator BufferTypeIterator;
			BufferType mBuffer( mDefaultBufferSize , uint8_t(0) );
			std::size_t lSize(0);

			// Just keep reading and, if we have not reached the EOF, extend the buffer and read some more.
			try{
				while( true ){
					lSize += boost::asio::read( socket, boost::asio::buffer( &mBuffer[lSize] , mDefaultBufferSize ) , error );
					if ( error ){
						if (error == boost::asio::error::eof ) {
							break;
						}else{
							throw boost::system::system_error(error);
						}
					}
					mBuffer.insert( mBuffer.end() , mDefaultBufferSize , uint8_t(0) );
				}
			}catch( std::exception& aExc ){
				std::cout << "Exception: " << aExc.what() << std::endl;
				return false;
			}	
			
			mBuffer.resize(lSize);

			//Parse the recieved data into an HttpResponseType object
			HttpResponseGrammar lGrammar2;
			boost::spirit::qi::phrase_parse( mBuffer.begin() , mBuffer.end() , lGrammar2 , boost::spirit::ascii::space , aResponse );
		
			if( DebugInfo ) 
			{
				std::cout << aResponse << std::endl;
			}

			if ( aResponse.method != "HTTP" || aResponse.status != 200)
			{
				return false;
			}

			
			
			return true;		
		}
	
	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
		
		

		
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{
	
		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , std::string& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.value();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , const char* aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.value();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , int32_t& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_int();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , uint32_t& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_uint();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , double& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_double();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}		
		
		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , float& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_float();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , bool& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_bool();
				return true;
			}else{
				if( DebugInfo ) std::cout << "Failed to get attribute \"" << aAttrName << "\"" << std::endl;
				return false;
			}
		}		

	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#endif
