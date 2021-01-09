/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#ifndef _uhal_utilities_files_hpp_
#define _uhal_utilities_files_hpp_


#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <wordexp.h>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/asio/error.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ip/udp.hpp>
#include <boost/asio/streambuf.hpp>
#include <boost/asio/read.hpp>
#include <boost/asio/write.hpp>
#include <boost/spirit/home/qi/parse.hpp>
#include <boost/spirit/include/qi_char_.hpp>
#include <boost/bind/bind.hpp>

#include "uhal/grammars/SemicolonDelimitedUriListGrammar.hpp"
#include "uhal/grammars/HttpResponseGrammar.hpp"
#include "uhal/grammars/URIGrammar.hpp"

#include "pugixml.hpp"

#include "uhal/log/log.hpp"
#include "uhal/log/exception.hpp"


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
  namespace exception
  {
    //! Exception class to handle the case where the string is not a comma-delimiter list of URIs.
    UHAL_DEFINE_EXCEPTION_CLASS ( UriListParsingError , "Exception class to handle the case where the string is not a comma-delimiter list of URIs." )
    //! Exception class to handle the case where a URI contains a non-supported protocol.
    UHAL_DEFINE_EXCEPTION_CLASS ( NonSupportedUriProtocol , "Exception class to handle the case where a URI contains a non-supported protocol." )
    //! Exception class to handle the case where a URI can not be opened.
    UHAL_DEFINE_EXCEPTION_CLASS ( CannotOpenFile , "Exception class to handle the case where a URI can not be opened." )
    //! Exception class to handle the case where a URI using the 'file://' protocol can not be expanded.
    UHAL_DEFINE_EXCEPTION_CLASS ( FileNotFound , "Exception class to handle the case where a URI using the 'file://' protocol can not be expanded." )
    //!Exception class to handle the case where expanding a shell expression failed.
    UHAL_DEFINE_EXCEPTION_CLASS ( ExpandingShellExpressionFailed , "Exception class to handle the case where expanding a shell expression failed." )
  }

  namespace utilities
  {
    /**
    	Parse a semicolon delimited list of URIs into a vector of protocol/address pairs
    	@param aSemicolonDelimitedUriList a string containing a semicolon delimited list of URIs
    	@param aUriList a vector to which the extracted protocol/address pairs are appended
    */
    void ParseSemicolonDelimitedUriList ( const std::string& aSemicolonDelimitedUriList , std::vector< std::pair<std::string, std::string> >& aUriList );
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
    	@param aParentPath a path which will be prepended to relative file names
    	@param aFiles a pointer to a vector of boost::filesystem::paths onto which the returned file names are appended
    */
    void ShellExpandFilenameExpr ( const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , std::vector< boost::filesystem::path >& aFiles );
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
      if ( DebugInfo )
      {
        try
        {
          log ( Info() , "Retrieving URL http://" , aURL );
        }
        catch ( const std::exception& aExc )
        {
          log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );						// Just debugging so although exception	is worrying, it is not critical
        }
      }

      std::pair<std::string, std::string> lURLPair;

      try
      {
        //split at the first slash
        boost::spirit::qi::phrase_parse ( aURL.begin() ,
                                          aURL.end() ,
                                          + ( boost::spirit::qi::char_ - "/" ) >> -boost::spirit::qi::lit ( "/" ) >> + ( boost::spirit::qi::char_ ) ,
                                          boost::spirit::ascii::space ,
                                          lURLPair );
      }
      catch ( const std::exception& aExc )
      {
        // log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
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
          throw boost::system::system_error ( lErrorCode );
        }
      }
      catch ( const std::exception& aExc )
      {
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
          throw boost::system::system_error ( lErrorCode );
        }
      }
      catch ( const std::exception& aExc )
      {
        return false;
      }

      // ... and get the reply. First we need a buffer to write the reply in to...
      static const int mDefaultBufferSize ( 65536 );
      typedef std::vector<uint8_t> BufferType;
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
              throw boost::system::system_error ( lErrorCode );
            }
          }

          mBuffer.insert ( mBuffer.end() , mDefaultBufferSize , uint8_t ( 0 ) );
        }
      }
      catch ( const std::exception& aExc )
      {
        return false;
      }

      mBuffer.resize ( lSize );
      //Parse the recieved data into an HttpResponseType object
      grammars::HttpResponseGrammar lGrammar2;
      boost::spirit::qi::phrase_parse ( mBuffer.begin() , mBuffer.end() , lGrammar2 , boost::spirit::ascii::space , aResponse );

      if ( DebugInfo )
      {
        try
        {
          log ( Info() , "HTTP response parsed as:\n" , aResponse );
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
    	@param aParentPath a path which will be prepended to relative file names
    	@param aBinder a callback function to be called on each file matching the linux shell expression
    */
    template < typename R , typename F , typename L>
    void OpenFileLocal ( const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , boost::_bi::bind_t<R,F,L> aBinder )
    {
      std::vector< boost::filesystem::path > lFilePaths;
      uhal::utilities::ShellExpandFilenameExpr ( aFilenameExpr , aParentPath , lFilePaths );

      for ( std::vector< boost::filesystem::path >::iterator lIt2 = lFilePaths.begin() ; lIt2 != lFilePaths.end() ; ++ lIt2 )
      {
        std::ifstream lStr ( lIt2->c_str() );

        if ( !lStr.is_open() )
        {
          uhal::exception::CannotOpenFile lExc;
          log ( lExc , "Failed to open file " , Quote ( lIt2->c_str() ) );
          throw lExc ;
        }
        else
        {
          lStr.seekg ( 0, std::ios::end );
          std::vector<uint8_t> lFile ( lStr.tellg() , 0 );
          lStr.seekg ( 0, std::ios::beg );
          lStr.read ( ( char* ) & ( lFile[0] ) , lFile.size() );
          aBinder ( std::string ( "file" ) , *lIt2 , boost::ref ( lFile ) );
        }

        lStr.close();
      }
    }


    /**
    	Given a URL, retrieve the file and call the callback function on each of them
    	@param aURL a URL to retrieve
    	@param aBinder a callback function to be called on the retrieved URL
    	@return success/failure status
    */
    template < typename R , typename F , typename L>
    void OpenFileHttp ( const std::string& aURL , boost::_bi::bind_t<R,F,L> aBinder )
    {
      HttpResponseType lHttpResponse;

      if ( ! uhal::utilities::HttpGet<true> ( aURL , lHttpResponse ) )
      {
        uhal::exception::CannotOpenFile lExc;
        log ( lExc , "Failed to download file " , Quote ( aURL ) );
        throw lExc;
      }

      boost::filesystem::path lFilePath = boost::filesystem::path ( aURL );
      aBinder ( std::string ( "http" ) , lFilePath , boost::ref ( lHttpResponse.content ) );
    }


    /**
    	Given a protocol and either a URL or a linux shell expression, open the file and call the callback function on each of them
    	@param aProtocol the protocol to be used to retrieve the file
    	@param aFilenameExpr a linux shell expression to be expanded or a URL to be retrieved
    	@param aParentPath a path which will be prepended to relative file names for local files. No meaning for http files.
    	@param aBinder a callback function to be called on the files
    	@return success/failure status
    */
    template < typename R , typename F , typename L>
    void OpenFile ( const std::string& aProtocol , const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , boost::_bi::bind_t<R,F,L> aBinder )
    {
      if ( aProtocol == "file" )
      {
        uhal::utilities::OpenFileLocal ( aFilenameExpr , aParentPath , aBinder );
      }
      else if ( aProtocol == "http" )
      {
        uhal::utilities::OpenFileHttp ( aFilenameExpr , aBinder );
      }
      else
      {
        uhal::exception::NonSupportedUriProtocol lExc;
        log ( lExc , "The protocol " , Quote ( aProtocol ) , " for file " , Quote ( aFilenameExpr ) , " is not supported." );
        log ( lExc , "The supported protocols are " , Quote ( "file://" ) , " and " , Quote ( "http://" ) );
        throw lExc;
      }
    }

  }
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#endif
