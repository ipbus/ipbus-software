#ifndef _uhal_Utilities_hpp_
#define _uhal_Utilities_hpp_

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>

#include <wordexp.h>

#include <boost/filesystem.hpp>
#include <boost/asio.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/bind/bind.hpp>

#include "BoostSpiritGrammars/SemicolonDelimitedUriListGrammar.hpp"
#include "BoostSpiritGrammars/HttpResponseGrammar.hpp"
#include "BoostSpiritGrammars/URLGrammar.hpp"

#include "pugixml/pugixml.hpp"

#include "uhal/log.hpp"

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{

		template < bool DebugInfo >
		void ParseSemicolonDelimitedUriList( const std::string& aSemicolonDelimitedUriList , std::vector< std::pair<std::string, std::string> >& aUriList )
		{
		
			BoostSpiritGrammars::SemicolonDelimitedUriListGrammar lGrammar;
			boost::spirit::qi::phrase_parse( aSemicolonDelimitedUriList.begin() , aSemicolonDelimitedUriList.end() , lGrammar , boost::spirit::ascii::space , aUriList );
			
			if( DebugInfo ){
				pantheios::log_INFORMATIONAL ( "Parsed \"" , aSemicolonDelimitedUriList , "\" to:" );
				for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = aUriList.begin() ; lIt != aUriList.end() ; ++lIt ){
					pantheios::log_INFORMATIONAL ( " > [" , lIt->first , "] \"" , lIt->second , "\"" );
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
				if( aFiles || aDirectories ) //pantheios::log_INFORMATIONAL ( "Shell expansion of \"" , aFilenameExpr , "\" returned:" );
				if( aFiles ){
					for( std::vector< boost::filesystem::path >::iterator lIt = aFiles->begin() ; lIt !=  aFiles->end() ; ++lIt ){
						pantheios::log_INFORMATIONAL ( " > [file] " , lIt->c_str() );				
					}
					
					if ( ! aFiles->size() ){
						pantheios::log_INFORMATIONAL ( " > No matching files." );
					}
				}
				if( aDirectories ){
					for( std::vector< boost::filesystem::path >::iterator lIt = aDirectories->begin() ; lIt !=  aDirectories->end() ; ++lIt ){
						pantheios::log_INFORMATIONAL ( " > [directory] " , lIt->c_str() );				
					}
						
					if ( ! aDirectories->size() ){
						pantheios::log_INFORMATIONAL ( " > No matching files." );
					}
				}					
			}
				
			wordfree(&lShellExpansion);		
		}
		

		template < bool DebugInfo >
		void ShellExpandFilenameExpr( const std::string& aFilenameExpr , std::vector< boost::filesystem::path > * aFiles = NULL , std::vector< boost::filesystem::path > * aDirectories = NULL )
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
			if( DebugInfo )
			{
				pantheios::log_INFORMATIONAL ( "Retrieving URL http://" , aURL );
			}
			
			BoostSpiritGrammars::URLGrammar lGrammar;
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
				pantheios::log_ERROR ( "Exception: " , aExc.what() );
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
				pantheios::log_ERROR ( "Exception: " , aExc.what() );
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
				pantheios::log_ERROR ( "Exception: " , aExc.what() );
				return false;
			}	
			
			mBuffer.resize(lSize);

			//Parse the recieved data into an HttpResponseType object
			BoostSpiritGrammars::HttpResponseGrammar lGrammar2;
			boost::spirit::qi::phrase_parse( mBuffer.begin() , mBuffer.end() , lGrammar2 , boost::spirit::ascii::space , aResponse );
		
			if( DebugInfo ) 
			{
				std::stringstream lStr;
				lStr << aResponse;
				pantheios::log_INFORMATIONAL ( lStr.str() );
			}

			if ( aResponse.method != "HTTP" || aResponse.status != 200 )
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
	
		template < typename R , typename F , typename L> 
		void OpenFileLocal( const std::string& aFilenameExpr , boost::_bi::bind_t<R,F,L> aBinder ){
			std::vector< boost::filesystem::path > lFilePaths;
			uhal::utilities::ShellExpandFilenameExpr<true>( aFilenameExpr , &lFilePaths );
		
			for( std::vector< boost::filesystem::path >::iterator lIt2 = lFilePaths.begin() ; lIt2 != lFilePaths.end() ; ++ lIt2 ){

				std::ifstream lStr( lIt2->c_str() );
				if ( !lStr.is_open() ){
					pantheios::log_ERROR ( "Failed to open \"" , lIt2->c_str() , "\". Continuing with next document for now but be aware!" );
				} else {
					lStr.seekg (0, std::ios::end);
					std::vector<uint8_t> lFile( lStr.tellg() , 0 );
					lStr.seekg (0, std::ios::beg);
					lStr.read ( (char*) &(lFile[0]) , lFile.size() );
					aBinder( std::string("file") , *lIt2 , boost::ref(lFile) );
				}
				lStr.close();	
			}
		}
		
		template < typename R , typename F , typename L> 
		void OpenFileHttp( const std::string& aURL , boost::_bi::bind_t<R,F,L> aBinder ){
			HttpResponseType lHttpResponse;
						
			if( ! uhal::utilities::HttpGet<true>( aURL , lHttpResponse ) ){
				pantheios::log_ERROR ( "Failed to download file " , aURL , ". Continuing for now but be aware!" );
				return;
			}
			
			boost::filesystem::path lFilePath = boost::filesystem::path( aURL );
			aBinder( std::string("http") , lFilePath , boost::ref(lHttpResponse.content) );
		}
				
		template < typename R , typename F , typename L> 
		void OpenFile( const std::string& aProtocol , const std::string& aFilenameExpr , boost::_bi::bind_t<R,F,L> aBinder ){
			if( aProtocol == "file" ){
				uhal::utilities::OpenFileLocal( aFilenameExpr , aBinder );
			}else if( aProtocol == "http" ){
				uhal::utilities::OpenFileHttp( aFilenameExpr , aBinder );	
			}else{
				pantheios::log_ERROR ( "Protocol \"" , aProtocol , "\" is unknown and I am, thus, ignoring file \"" , aFilenameExpr , "\". Continuing for now but be aware!" );
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
		void PugiXMLParseResultPrettifier( const pugi::xml_parse_result& aLoadResult , const boost::filesystem::path& aPath , const std::vector<uint8_t>& aFile );		
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
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , const char* aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.value();
				return true;
			}else{
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , int32_t& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_int();
				return true;
			}else{
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , uint32_t& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_uint();
				return true;
			}else{
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , double& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_double();
				return true;
			}else{
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}		
		
		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , float& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_float();
				return true;
			}else{
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}

		template < bool DebugInfo >
		bool GetXMLattribute( const pugi::xml_node& aNode , const char* aAttrName , bool& aTarget ){
			if( pugi::xml_attribute lAttr = aNode.attribute( aAttrName ) ){
				aTarget = lAttr.as_bool();
				return true;
			}else{
				if( DebugInfo ) pantheios::log_ERROR ( "Failed to get attribute \"" , aAttrName , "\"" );
				return false;
			}
		}		

	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#endif
