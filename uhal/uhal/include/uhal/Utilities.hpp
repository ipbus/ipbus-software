/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_Utilities_hpp_
#define _uhal_Utilities_hpp_

#include <vector>
#include <string>
#include <iostream>
#include <fstream>

#include <wordexp.h>

#include <boost/filesystem.hpp>
#include <boost/asio.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/bind/bind.hpp>

#include "BoostSpiritGrammars/SemicolonDelimitedUriListGrammar.hpp"
#include "BoostSpiritGrammars/HttpResponseGrammar.hpp"
#include "BoostSpiritGrammars/URLGrammar.hpp"

#include "pugixml/pugixml.hpp"

#include "log/log.hpp"
#include "uhal/exception.hpp"

#ifdef __GNUC__
#include <ext/hash_map>
#else
#include <hash_map>
#endif

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace std
{
	using namespace __gnu_cxx;
}

//! Working in the __gnu_cxx namespace
namespace __gnu_cxx
{
	//! Add a hash function for a c++ std::string
	template<> struct hash< std::string >
	{
		//! implement the hash by calling the hash for the equivalent c-string
		size_t operator() ( const std::string& x ) const
		{
			using namespace uhal;
		
			try
			{
				return hash< const char* >() ( x.c_str() );
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}
	};
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{
		/**
			Parse a semicolon delimited list of URIs into a vector of protocol/address pairs
			@param aSemicolonDelimitedUriList a string containing a semicolon delimited list of URIs
			@param aUriList a vector to which the extracted protocol/address pairs are appended
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool ParseSemicolonDelimitedUriList ( const std::string& aSemicolonDelimitedUriList , std::vector< std::pair<std::string, std::string> >& aUriList )
		{
			try
			{
				BoostSpiritGrammars::SemicolonDelimitedUriListGrammar lGrammar;
				boost::spirit::qi::phrase_parse ( aSemicolonDelimitedUriList.begin() , aSemicolonDelimitedUriList.end() , lGrammar , boost::spirit::ascii::space , aUriList );
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				log ( Error() , "Expression \"" , aSemicolonDelimitedUriList , "\" must be a semicolon delimeted list and all files must be in the form \"protocol://address\"" );
				return false;
			}

			if ( DebugInfo )
			{
				try
				{
					log ( Notice() , "Parsed \"" , aSemicolonDelimitedUriList , "\" to:" );

					for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = aUriList.begin() ; lIt != aUriList.end() ; ++lIt )
					{
						log ( Notice() , " > [" , lIt->first , "] \"" , lIt->second , "\"" );
					}
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
					// Just debugging so although exception	is worrying, it is not critical
				}
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
		/**
			Perform shell expansion of a linux shell expression ( e.g. "~/c*.xml" -> "/usr/home/awr/connections.xml" ) and convert into boost::filesystem::paths
			@param aFilenameExpr a c-style string containing a linux shell expression to be expanded
			@param aFiles a pointer to a vector of boost::filesystem::paths onto which the returned file names are appended
			@param aFiles a pointer to a vector of boost::filesystem::paths onto which the returned directory names are appended
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool ShellExpandFilenameExpr ( const char* aFilenameExpr , std::vector< boost::filesystem::path > * aFiles = NULL , std::vector< boost::filesystem::path > * aDirectories = NULL )
		{
			if ( !aFiles && !aDirectories )
			{
				// We have nowhere to write the data so don't bother expanding the expression
				return true;
			}

			try
			{
				//struct which will store the shell expansions of the expression
				wordexp_t lShellExpansion;
				wordexp ( aFilenameExpr , &lShellExpansion , 0 );

				for ( std::size_t i = 0 ; i != lShellExpansion.we_wordc ; i++ )
				{
					boost::filesystem::path lPath ( lShellExpansion.we_wordv[i] );

					if ( boost::filesystem::exists ( lPath ) )
					{
						if ( aFiles )
						{
							if ( boost::filesystem::is_regular_file ( lPath ) )
							{
								aFiles->push_back ( boost::filesystem::absolute ( lPath ) );
							}
						}

						if ( aDirectories )
						{
							if ( boost::filesystem::is_directory ( lPath ) )
							{
								aDirectories->push_back ( boost::filesystem::absolute ( lPath ) );
							}
						}
					}
				}

				wordfree ( &lShellExpansion );
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				return false;
			}

			// Don't bother with Logging if the logger won't include it!
			Notice lLoggingLevel;
			if( log_configuration::LoggingIncludes( lLoggingLevel ) ){
			
				// After that we listen to the user's preference
				if ( DebugInfo )
				{
					try
					{
						if ( aFiles || aDirectories )
						{
							log ( lLoggingLevel , "Shell expansion of \"" , aFilenameExpr , "\" returned:" );
						}

						if ( aFiles )
						{
								for ( std::vector< boost::filesystem::path >::iterator lIt = aFiles->begin() ; lIt !=  aFiles->end() ; ++lIt )
								{
									log ( lLoggingLevel , " > [file] " , lIt->c_str() );
								}

								if ( ! aFiles->size() )
								{
									log ( lLoggingLevel , " > No matching files." );
								}
						}

						if ( aDirectories )
						{
							for ( std::vector< boost::filesystem::path >::iterator lIt = aDirectories->begin() ; lIt !=  aDirectories->end() ; ++lIt )
							{
								log ( lLoggingLevel , " > [directory] " , lIt->c_str() );
							}

							if ( ! aDirectories->size() )
							{
								log ( lLoggingLevel , " > No matching directories." );
							}
						}
					}
					catch ( const std::exception& aExc )
					{
						log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
						// Just debugging so although exception	is worrying, it is not critical
					}
				}
			}

			return true;
		}

		/**
			Perform shell expansion of a linux shell expression ( e.g. "~/c*.xml" -> "/usr/home/awr/connections.xml" ) and convert into boost::filesystem::paths
			@param aFilenameExpr a string containing a linux shell expression to be expanded
			@param aFiles a pointer to a vector of boost::filesystem::paths onto which the returned file names are appended
			@param aFiles a pointer to a vector of boost::filesystem::paths onto which the returned directory names are appended
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool ShellExpandFilenameExpr ( const std::string& aFilenameExpr , std::vector< boost::filesystem::path > * aFiles = NULL , std::vector< boost::filesystem::path > * aDirectories = NULL )
		{
			try
			{
				return ShellExpandFilenameExpr< DebugInfo > ( aFilenameExpr.c_str() , aFiles , aDirectories );
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
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
		/**
			Retrieve a file by HTTP
			@param aURL a URL to retrieve
			@param aResponse a structure into which the returned HTTP packet is parsed
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool HttpGet ( const std::string& aURL , HttpResponseType& aResponse )
		{
			try
			{
				if ( DebugInfo )
				{
					try
					{
						log ( Notice() , "Retrieving URL http://" , aURL );
					}
					catch ( const std::exception& aExc )
					{
						log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
						// Just debugging so although exception	is worrying, it is not critical
					}
				}

				std::pair<std::string, std::string> lURLPair;

				try
				{
					BoostSpiritGrammars::URIGrammarShort lGrammar;
					boost::spirit::qi::phrase_parse ( aURL.begin() , aURL.end() , lGrammar , boost::spirit::ascii::space , lURLPair );
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
					return false;
				}

				boost::system::error_code lErrorCode ( boost::asio::error::host_not_found );
				// The IO service everything will go through
				boost::asio::io_service io_service;
				// Get a list of endpoints corresponding to the server name.
				boost::asio::ip::tcp::resolver resolver ( io_service );
				boost::asio::ip::tcp::resolver::query query ( lURLPair.first , "http" );
				boost::asio::ip::tcp::resolver::iterator endpoint_iterator = resolver.resolve ( query );
				boost::asio::ip::tcp::resolver::iterator end;
				// Try each endpoint until we successfully establish a connection.
				boost::asio::ip::tcp::socket socket ( io_service );

				try
				{
					while ( lErrorCode && endpoint_iterator != end )
					{
						socket.close();
						socket.connect ( *endpoint_iterator++, lErrorCode );
					}

					if ( lErrorCode )
					{
						log ( Error() , "Throwing at " , ThisLocation() );
						throw boost::system::system_error ( lErrorCode );
					}
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
					return false;
				}

				// Form the request. We specify the "Connection: close" header so that the server will close the socket after transmitting the response. This will allow us to treat all data up until the EOF as the content.
				boost::asio::streambuf request;
				std::ostream request_stream ( &request );
				request_stream << "GET /" << lURLPair.second << " HTTP/1.0\r\n";
				request_stream << "Host: " << lURLPair.first << "\r\n";
				request_stream << "Accept: */*\r\n";
				request_stream << "Connection: close\r\n\r\n";

				try
				{
					// Send the request...
					boost::asio::write ( socket , request , lErrorCode );

					if ( lErrorCode )
					{
						log ( Error() , "Throwing at " , ThisLocation() );
						throw boost::system::system_error ( lErrorCode );
					}
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
					return false;
				}

				// ... and get the reply. First we need a buffer to write the reply in to...
				static const int mDefaultBufferSize ( 65536 );
				typedef std::vector<uint8_t> BufferType;
				typedef BufferType::iterator BufferTypeIterator;
				BufferType mBuffer ( mDefaultBufferSize , uint8_t ( 0 ) );
				std::size_t lSize ( 0 );

				// Just keep reading and, if we have not reached the EOF, extend the buffer and read some more.
				try
				{
					while ( true )
					{
						lSize += boost::asio::read ( socket, boost::asio::buffer ( &mBuffer[lSize] , mDefaultBufferSize ) , lErrorCode );

						if ( lErrorCode )
						{
							if ( lErrorCode == boost::asio::error::eof )
							{
								break;
							}
							else
							{
								log ( Error() , "Throwing at " , ThisLocation() );
								throw boost::system::system_error ( lErrorCode );
							}
						}

						mBuffer.insert ( mBuffer.end() , mDefaultBufferSize , uint8_t ( 0 ) );
					}
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
					return false;
				}

				mBuffer.resize ( lSize );
				//Parse the recieved data into an HttpResponseType object
				BoostSpiritGrammars::HttpResponseGrammar lGrammar2;
				boost::spirit::qi::phrase_parse ( mBuffer.begin() , mBuffer.end() , lGrammar2 , boost::spirit::ascii::space , aResponse );

				if ( DebugInfo )
				{
					try
					{
						log ( Notice() , "HTTP response parsed as:\n" , aResponse );
					}
					catch ( const std::exception& aExc )
					{
						log ( Error() , "EXCEPTION: " , aExc.what() , " caught in " , ThisLocation() , " Continuing." );
						// Just debugging so although exception	is worrying, it is not critical
					}
				}

				if ( aResponse.method != "HTTP" || aResponse.status != 200 )
				{
					return false;
				}

				return true;
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
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
		/**
			Given a linux shell expression, open all files which match it and call the callback function on each of them
			@param aFilenameExpr a linux shell expression to be expanded
			@param aBinder a callback function to be called on each file matching the linux shell expression
			@return success/failure status
		*/
		template < typename R , typename F , typename L>
		bool OpenFileLocal ( const std::string& aFilenameExpr , boost::_bi::bind_t<R,F,L> aBinder )
		{
			try
			{
				std::vector< boost::filesystem::path > lFilePaths;

				if ( !uhal::utilities::ShellExpandFilenameExpr<true> ( aFilenameExpr , &lFilePaths ) )
				{
					return false;
				}

				for ( std::vector< boost::filesystem::path >::iterator lIt2 = lFilePaths.begin() ; lIt2 != lFilePaths.end() ; ++ lIt2 )
				{
					std::ifstream lStr ( lIt2->c_str() );

					if ( !lStr.is_open() )
					{
						log ( Error() , "Failed to open " , lIt2->c_str() , ". Continuing with next document for now but be aware!" );
					}
					else
					{
						try
						{
							lStr.seekg ( 0, std::ios::end );
							std::vector<uint8_t> lFile ( lStr.tellg() , 0 );
							lStr.seekg ( 0, std::ios::beg );
							lStr.read ( ( char* ) & ( lFile[0] ) , lFile.size() );
							aBinder ( std::string ( "file" ) , *lIt2 , boost::ref ( lFile ) );
						}
						catch ( const std::exception& aExc )
						{
							log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
							throw uhal::exception ( aExc );
						}
					}

					lStr.close();
				}

				return true;
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Given a URL, retrieve the file and call the callback function on each of them
			@param aURL a URL to retrieve
			@param aBinder a callback function to be called on the retrieved URL
			@return success/failure status
		*/
		template < typename R , typename F , typename L>
		bool OpenFileHttp ( const std::string& aURL , boost::_bi::bind_t<R,F,L> aBinder )
		{
			try
			{
				HttpResponseType lHttpResponse;

				if ( ! uhal::utilities::HttpGet<true> ( aURL , lHttpResponse ) )
				{
					log ( Error() , "Failed to download file " , aURL , ". Continuing for now but be aware!" );
					return false;
				}

				try
				{
					boost::filesystem::path lFilePath = boost::filesystem::path ( aURL );
					aBinder ( std::string ( "http" ) , lFilePath , boost::ref ( lHttpResponse.content ) );
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
					throw uhal::exception ( aExc );
				}

				return true;
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Given a protocol and either a URL or a linux shell expression, open the file and call the callback function on each of them
			@param aProtocol the protocol to be used to retrieve the file
			@param aFilenameExpr a linux shell expression to be expanded or a URL to be retrieved
			@param aBinder a callback function to be called on the files
			@return success/failure status
		*/
		template < typename R , typename F , typename L>
		bool OpenFile ( const std::string& aProtocol , const std::string& aFilenameExpr , boost::_bi::bind_t<R,F,L> aBinder )
		{
			try
			{
				if ( aProtocol == "file" )
				{
					return uhal::utilities::OpenFileLocal ( aFilenameExpr , aBinder );
				}
				else if ( aProtocol == "http" )
				{
					return uhal::utilities::OpenFileHttp ( aFilenameExpr , aBinder );
				}
				else
				{
					log ( Error() , "Protocol \"" , aProtocol , "\" is unknown and I am, thus, ignoring file \"" , aFilenameExpr , "\". Continuing for now but be aware!" );
					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
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
		/**
			Helper function to make debugging failures when parsing XML files easier
			@param aLoadResult the result of the parsing
			@param aPath the full filename of file whose parsing failed
			@param aFile a byte vector containing the contents of the file (stored like this because the file could be either local or retrieved by HTTP)
		*/
		void PugiXMLParseResultPrettifier ( const pugi::xml_parse_result& aLoadResult , const boost::filesystem::path& aPath , const std::vector<uint8_t>& aFile );
	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{
		/**
			Helper function to calculate the number of zero-bits at the righthand end of a 32-bit number
			@param aValue a 32-bit number whose trailing zero-bits are to be counted
			@return the number of trailing zero-bits
		*/
		unsigned int TrailingRightBits ( uint32_t aValue );
	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , std::string& aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					aTarget = lAttr.value();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , const char* aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					aTarget = lAttr.value();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , int32_t& aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					std::string lAttrStr ( lAttr.value() );
					std::stringstream ss;

					//if string is of the form "x89abcdef" , "X89abcdef" , "0x89abcdef" , "0X89abcdef"
					if ( lAttrStr.size() > 2 )
					{
						if ( ( lAttrStr[1] == 'x' ) || ( lAttrStr[1] == 'X' ) )
						{
							ss << std::hex << lAttrStr.substr ( 2 );
						}
						else
						{
							ss << lAttrStr;
						}
					}
					else if ( lAttrStr.size() > 1 )
					{
						if ( ( lAttrStr[0] == 'x' ) || ( lAttrStr[0] == 'X' ) )
						{
							ss << std::hex << lAttrStr.substr ( 1 );
						}
						else
						{
							ss << lAttrStr;
						}
					}
					else
					{
						ss << lAttrStr;
					}

					ss >> aTarget;
					// aTarget = lAttr.as_int();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , uint32_t& aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					std::string lAttrStr ( lAttr.value() );
					std::stringstream ss;

					//if string is of the form "x89abcdef" , "X89abcdef" , "0x89abcdef" , "0X89abcdef"
					if ( lAttrStr.size() > 2 )
					{
						if ( ( lAttrStr[1] == 'x' ) || ( lAttrStr[1] == 'X' ) )
						{
							ss << std::hex << lAttrStr.substr ( 2 );
						}
						else
						{
							ss << lAttrStr;
						}
					}
					else if ( lAttrStr.size() > 1 )
					{
						if ( ( lAttrStr[0] == 'x' ) || ( lAttrStr[0] == 'X' ) )
						{
							ss << std::hex << lAttrStr.substr ( 1 );
						}
						else
						{
							ss << lAttrStr;
						}
					}
					else
					{
						ss << lAttrStr;
					}

					ss >> aTarget;
					// aTarget = lAttr.as_uint();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , double& aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					aTarget = lAttr.as_double();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , float& aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					aTarget = lAttr.as_float();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

		/**
			Helper function to retrieve a named attribute from a PugiXML node and cast it to the correct type
			@param aNode a node from which the attribute is to be extracted
			@param aAttrName the name of the attribute to be extracted
			@param aTarget a variable into which the attribute's value id to be written
			@return success/failure status
		*/
		template < bool DebugInfo >
		bool GetXMLattribute ( const pugi::xml_node& aNode , const char* aAttrName , bool& aTarget )
		{
			try
			{
				pugi::xml_attribute lAttr = aNode.attribute ( aAttrName );

				if ( ! lAttr.empty() )
				{
					aTarget = lAttr.as_bool();
					return true;
				}
				else
				{
					if ( DebugInfo )
					{
						log ( Error() , "Failed to get attribute \"" , aAttrName , "\"" );
					}

					return false;
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

	}
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#endif
