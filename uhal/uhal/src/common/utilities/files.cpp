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

      for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = aUriList.begin() ; lIt != aUriList.end() ; ++lIt )
      {
        log ( Debug() , "    > [" , lIt->first , "] " , Quote ( lIt->second ) );
      }
    }


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

        for ( std::vector< boost::filesystem::path >::iterator lIt = aFiles.begin() ; lIt !=  aFiles.end() ; ++lIt )
        {
          log ( Debug() , " > [file] " , lIt->c_str() );
        }
      }
    }
  }
}

