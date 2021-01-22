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

#include "uhal/utilities/files.hpp"


#include <fstream>
#include <iostream>

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
#include <boost/spirit/include/qi.hpp>

#include "uhal/log/log.hpp"


namespace uhal
{
  namespace utilities
  {

    void ParseSemicolonDelimitedUriList ( const std::string& aSemicolonDelimitedUriList , std::vector< std::pair<std::string, std::string> >& aUriList )
    {
      try
      {
        grammars::SemicolonDelimitedUriListGrammar lGrammar;
        boost::spirit::qi::phrase_parse ( aSemicolonDelimitedUriList.begin() , aSemicolonDelimitedUriList.end() , lGrammar , boost::spirit::ascii::space , aUriList );
      }
      catch ( const std::exception& aExc )
      {
        exception::UriListParsingError lExc;
        log ( lExc , "Expression " , Quote ( aSemicolonDelimitedUriList ) , " must be a semicolon delimeted list and all files must be in the form " , Quote ( "protocol://address" ) );
        throw lExc;
      }

      log ( Debug() , "Parsed " , Quote ( aSemicolonDelimitedUriList ) , " to:" );

      for (const auto& x: aUriList)
      {
        log ( Debug() , "    > [" , x.first , "] " , Quote ( x.second ) );
      }
    }


    void ShellExpandFilenameExpr ( const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , std::vector< boost::filesystem::path >& aFiles )
    {
      try
      {
        //struct which will store the shell expansions of the expression
        wordexp_t lShellExpansion;
        wordexp ( aFilenameExpr.c_str() , &lShellExpansion , 0 );

        for ( std::size_t i = 0 ; i != lShellExpansion.we_wordc ; i++ )
        {
          boost::filesystem::path lPath ( lShellExpansion.we_wordv[i] );
          log ( Debug() , "lPath was " , Quote ( lPath.c_str() ) );
          log ( Debug() , "aParentPath is " , Quote ( aParentPath.c_str() ) );
          lPath = boost::filesystem::absolute ( lPath , aParentPath );
          log ( Debug() , "lPath now " , Quote ( lPath.c_str() ) );

          if ( boost::filesystem::exists ( lPath ) )
          {
            if ( boost::filesystem::is_regular_file ( lPath ) )
            {
              aFiles.push_back ( lPath );
            }
          }
        }

        wordfree ( &lShellExpansion );
      }
      catch ( const std::exception& aExc )
      {
        uhal::exception::ExpandingShellExpressionFailed lExc;
        log ( lExc , "Caught exception during shell expansion: " , Quote ( aExc.what() ) );
        throw lExc;
      }

      if ( ! aFiles.size() )
      {
        uhal::exception::FileNotFound lExc;
        log ( lExc , "No matching files for expression " , Quote ( aFilenameExpr ) , " with parent path " , Quote ( aParentPath.c_str() ) );
        throw lExc;
      }
      else
      {
        log ( Debug() , "Shell expansion of " , Quote ( aFilenameExpr.c_str() ) , " returned:" );

        for (const auto& lFile: aFiles)
          log ( Debug() , " > [file] " , lFile.c_str() );
      }
    }


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
          log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );            // Just debugging so although exception is worrying, it is not critical
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
          // Just debugging so although exception is worrying, it is not critical
        }
      }

      if ( aResponse.method != "HTTP" || aResponse.status != 200 )
      {
        return false;
      }

      return true;
    }


    template bool HttpGet<false> ( const std::string& , HttpResponseType& );
    template bool HttpGet<true> ( const std::string& , HttpResponseType& );


    void OpenFileLocal ( const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , const detail::FileCallback_t& aCallback)
    {
      std::vector< boost::filesystem::path > lFilePaths;
      uhal::utilities::ShellExpandFilenameExpr ( aFilenameExpr , aParentPath , lFilePaths );

      for (const auto& lPath: lFilePaths)
      {
        std::ifstream lStr ( lPath.c_str() );

        if ( !lStr.is_open() )
        {
          uhal::exception::CannotOpenFile lExc;
          log ( lExc , "Failed to open file " , Quote ( lPath.c_str() ) );
          throw lExc ;
        }
        else
        {
          lStr.seekg ( 0, std::ios::end );
          std::vector<uint8_t> lFile ( lStr.tellg() , 0 );
          lStr.seekg ( 0, std::ios::beg );
          lStr.read ( ( char* ) & ( lFile[0] ) , lFile.size() );
          aCallback ( std::string ( "file" ) , lPath , lFile);
        }

        lStr.close();
      }
    }


    void OpenFileHttp ( const std::string& aURL , const detail::FileCallback_t& aCallback )
    {
      HttpResponseType lHttpResponse;

      if ( ! uhal::utilities::HttpGet<true> ( aURL , lHttpResponse ) )
      {
        uhal::exception::CannotOpenFile lExc;
        log ( lExc , "Failed to download file " , Quote ( aURL ) );
        throw lExc;
      }

      boost::filesystem::path lFilePath = boost::filesystem::path ( aURL );
      aCallback ( std::string ( "http" ) , lFilePath , lHttpResponse.content );
    }


    void OpenFile ( const std::string& aProtocol , const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , const detail::FileCallback_t& aCallback )
    {
      if ( aProtocol == "file" )
      {
        uhal::utilities::OpenFileLocal ( aFilenameExpr , aParentPath , aCallback );
      }
      else if ( aProtocol == "http" )
      {
        uhal::utilities::OpenFileHttp ( aFilenameExpr , aCallback );
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

