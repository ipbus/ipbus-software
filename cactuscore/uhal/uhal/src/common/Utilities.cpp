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

---------------------------------------------------------------------------
*/

#include "uhal/Utilities.hpp"

#include "uhal/log/log.hpp"

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

      for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = aUriList.begin() ; lIt != aUriList.end() ; ++lIt )
      {
        log ( Debug() , "    > [" , lIt->first , "] " , Quote ( lIt->second ) );
      }
    }

    void PugiXMLParseResultPrettifier ( const pugi::xml_parse_result& aLoadResult , const boost::filesystem::path& aPath , const std::vector<uint8_t>& aFile )
    {
      log ( Error() , "Failed to parse file " , aPath.c_str() , ". PugiXML returned the following description " , Quote ( aLoadResult.description() ) , "." );
      std::size_t lLineCounter ( 1 );
      std::vector<uint8_t>::const_iterator lIt0 ( aFile.begin() );
      std::vector<uint8_t>::const_iterator lIt1 ( aFile.begin() + aLoadResult.offset );
      std::vector<uint8_t>::const_iterator lIt2 ( lIt1 );

      for ( ; lIt0!=lIt1 ; ++lIt0 )
      {
        if ( *lIt0 == '\n' )
        {
          lLineCounter++;
        }
      }

      for ( ; lIt1 != aFile.begin() ; --lIt1 )
      {
        if ( *lIt1 == '\n' )
        {
          ++lIt1;
          break;
        }
      }

      for ( ; lIt2 != aFile.end() ; ++lIt2 )
      {
        if ( *lIt2 == '\n' )
        {
          --lIt2;
          break;
        }
      }

      std::size_t lDist0 ( lIt0 - lIt1 );
      std::size_t lDist1 ( lIt2 - lIt0 );
      std::string lLine;
      lLine.reserve ( lIt2 - lIt1 );

      for ( ; lIt1 != lIt2 ; ++lIt1 )
      {
        if ( isprint ( *lIt1 ) || *lIt1==10 )
        {
          lLine += *lIt1;
        }
        else
        {
          lLine += '#';
        }
      }

      log ( Error() , "Error occured at line number " , Integer ( lLineCounter ) ,
            ", character " , Integer ( lDist0+1 ) , "\n"
            "LINE           : " , lLine , "\n"
            "ERROR LOCATION : " , std::string ( lDist0 , '_' ) , "^" , std::string ( lDist1 , '_' ) );
    }

  }
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
  namespace utilities
  {
    void ShellExpandFilenameExpr ( const std::string& aFilenameExpr , const boost::filesystem::path& aParentPath , std::vector< boost::filesystem::path >& aFiles )
    {
      try
      {
        //	boost::lock_guard<boost::mutex> lLock ( gUtilityMutex );
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
        log ( lExc , "Caught exception: " , Quote ( aExc.what() ) );
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

        for ( std::vector< boost::filesystem::path >::iterator lIt = aFiles.begin() ; lIt !=  aFiles.end() ; ++lIt )
        {
          log ( Debug() , " > [file] " , lIt->c_str() );
        }
      }
    }
  }
}


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
  namespace utilities
  {
    unsigned int TrailingRightBits ( uint32_t aValue )
    {
      // unsigned int lReturn = sizeof ( aValue ) * 8; // lReturn will be the number of zero bits on the right
      unsigned int lReturn = sizeof ( aValue ) << 3; // lReturn will be the number of zero bits on the right
      aValue &= -signed ( aValue );

      if ( aValue )
      {
        lReturn--;
      }

      if ( aValue & 0x0000FFFF )
      {
        lReturn -= 16;
      }

      if ( aValue & 0x00FF00FF )
      {
        lReturn -= 8;
      }

      if ( aValue & 0x0F0F0F0F )
      {
        lReturn -= 4;
      }

      if ( aValue & 0x33333333 )
      {
        lReturn -= 2;
      }

      if ( aValue & 0x55555555 )
      {
        lReturn -= 1;
      }

      return lReturn;
    }
  }
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
