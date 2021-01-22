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

#include "uhal/utilities/xml.hpp"


#include <ctype.h>
#include <sstream>

#include "pugixml.hpp"

#include "uhal/log/LogLevels.hpp"
#include "uhal/log/log.hpp"
#include "uhal/log/log_inserters.integer.hpp"
#include "uhal/log/log_inserters.quote.hpp"



// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
  namespace utilities
  {

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


    template < bool DebugInfo >
    bool GetXMLattribute ( const pugi::xml_node& aNode , const std::string& aAttrName , std::string& aTarget )
    {
      pugi::xml_attribute lAttr = aNode.attribute ( aAttrName.c_str() );

      if ( ! lAttr.empty() )
      {
        aTarget = lAttr.value();
        return true;
      }
      else
      {
        if ( DebugInfo )
        {
          log ( Error() , "Failed to get attribute " , Quote ( aAttrName ) , " from XML node." );
        }

        return false;
      }
    }

    template bool GetXMLattribute<true>( const pugi::xml_node& aNode , const std::string& aAttrName , std::string& aTarget );
    template bool GetXMLattribute<false>( const pugi::xml_node& aNode , const std::string& aAttrName , std::string& aTarget );


    template < bool DebugInfo >
    bool GetXMLattribute ( const pugi::xml_node& aNode , const std::string& aAttrName , int32_t& aTarget )
    {
      pugi::xml_attribute lAttr = aNode.attribute ( aAttrName.c_str() );

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

        // ss >> aTarget;
        // aTarget = lAttr.as_int();

        if ( ss.str().size() > 10 )
        {
          exception::StringNumberWillNotFitInto32BitNumber lExc;
          log ( lExc , "XML attribute " , Quote ( aAttrName ) , " has value " , Quote ( ss.str() ) , " which is too big to fit into 32-bit number" );
          throw lExc;
        }

        int64_t lTarget;
        ss >> lTarget;

        if ( lTarget>>32 )
        {
          exception::StringNumberWillNotFitInto32BitNumber lExc;
          log ( lExc , "XML attribute " , Quote ( aAttrName ) , " has value " , Quote ( ss.str() ) , " which is too big to fit into 32-bit number" );
          throw lExc;
        }

        aTarget = ( int32_t ) ( lTarget );
        return true;
      }
      else
      {
        if ( DebugInfo )
        {
          log ( Error() , "Failed to get attribute " , Quote ( aAttrName ) , " from XML node." );
        }

        return false;
      }
    }

    template bool GetXMLattribute<true>( const pugi::xml_node& aNode , const std::string& aAttrName , int32_t& aTarget );
    template bool GetXMLattribute<false>( const pugi::xml_node& aNode , const std::string& aAttrName , int32_t& aTarget );


    template < bool DebugInfo >
    bool GetXMLattribute ( const pugi::xml_node& aNode , const std::string& aAttrName , uint32_t& aTarget )
    {
      pugi::xml_attribute lAttr = aNode.attribute ( aAttrName.c_str() );

      if ( ! lAttr.empty() )
      {
        std::string lAttrStr ( lAttr.value() );
        std::stringstream ss;

        size_t lBasePrefixLength = 0;

        if ( lAttrStr.empty() )
          throw exception::NodeAttributeIncorrectValue("XML attribute '" + aAttrName + "' is empty, so cannot be converted to uint32_t");

        //if string is of the form "x89abcdef" , "X89abcdef" , "0x89abcdef" , "0X89abcdef"
        if ( lAttrStr.size() > 2 )
        {
          if ( ( lAttrStr[0] == '0' ) and ( ( lAttrStr[1] == 'x' ) or ( lAttrStr[1] == 'X' ) ) )
          {
            lBasePrefixLength = 2;
            ss << std::hex;
          }
        }

        if ( lAttrStr.size() > 1 )
        {
          if ( ( lAttrStr[0] == 'x' ) || ( lAttrStr[0] == 'X' ) )
          {
            lBasePrefixLength = 1;
            ss << std::hex;
          }
        }

        if ( lAttrStr.find_first_not_of(lBasePrefixLength ? "0123456789abcdefABCDEF" : "0123456789", lBasePrefixLength) != std::string::npos )
          throw exception::NodeAttributeIncorrectValue("XML attribute '" + aAttrName + "' has value '" + lAttrStr + "' that cannot be converted to uint32_t");

        ss << lAttrStr.substr(lBasePrefixLength);
        

        if ( ss.str().size() > 10 )
        {
          exception::StringNumberWillNotFitInto32BitNumber lExc;
          log ( lExc , "XML attribute " , Quote ( aAttrName ) , " has value " , Quote ( ss.str() ) , " which is too big to fit into 32-bit number" );
          throw lExc;
        }

        uint64_t lTarget;
        ss >> lTarget;

        if ( lTarget>>32 )
        {
          exception::StringNumberWillNotFitInto32BitNumber lExc;
          log ( lExc , "XML attribute " , Quote ( aAttrName ) , " has value " , Quote ( ss.str() ) , " which is too big to fit into 32-bit number" );
          throw lExc;
        }

        aTarget = ( uint32_t ) ( lTarget );
        return true;
      }
      else
      {
        if ( DebugInfo )
        {
          log ( Error() , "Failed to get attribute " , Quote ( aAttrName ) , " from XML node." );
        }

        return false;
      }
    }

    template bool GetXMLattribute<true>( const pugi::xml_node& aNode , const std::string& aAttrName , uint32_t& aTarget );
    template bool GetXMLattribute<false>( const pugi::xml_node& aNode , const std::string& aAttrName , uint32_t& aTarget );


    template < bool DebugInfo >
    bool GetXMLattribute ( const pugi::xml_node& aNode , const std::string& aAttrName , double& aTarget )
    {
      pugi::xml_attribute lAttr = aNode.attribute ( aAttrName.c_str() );

      if ( ! lAttr.empty() )
      {
        aTarget = lAttr.as_double();
        return true;
      }
      else
      {
        if ( DebugInfo )
        {
          log ( Error() , "Failed to get attribute " , Quote ( aAttrName ) , " from XML node." );
        }

        return false;
      }
    }

    template bool GetXMLattribute<true>( const pugi::xml_node& aNode , const std::string& aAttrName , double& aTarget );
    template bool GetXMLattribute<false>( const pugi::xml_node& aNode , const std::string& aAttrName , double& aTarget );


    template < bool DebugInfo >
    bool GetXMLattribute ( const pugi::xml_node& aNode , const std::string& aAttrName , float& aTarget )
    {
      pugi::xml_attribute lAttr = aNode.attribute ( aAttrName.c_str() );

      if ( ! lAttr.empty() )
      {
        aTarget = lAttr.as_float();
        return true;
      }
      else
      {
        if ( DebugInfo )
        {
          log ( Error() , "Failed to get attribute " , Quote ( aAttrName ) , " from XML node." );
        }

        return false;
      }
    }

    template bool GetXMLattribute<true>( const pugi::xml_node& aNode , const std::string& aAttrName , float& aTarget );
    template bool GetXMLattribute<false>( const pugi::xml_node& aNode , const std::string& aAttrName , float& aTarget );


    template < bool DebugInfo >
    bool GetXMLattribute ( const pugi::xml_node& aNode , const std::string& aAttrName , bool& aTarget )
    {
      pugi::xml_attribute lAttr = aNode.attribute ( aAttrName.c_str() );

      if ( ! lAttr.empty() )
      {
        aTarget = lAttr.as_bool();
        return true;
      }
      else
      {
        if ( DebugInfo )
        {
          log ( Error() , "Failed to get attribute " , Quote ( aAttrName ) , " from XML node." );
        }

        return false;
      }
    }

    template bool GetXMLattribute<true>( const pugi::xml_node& aNode , const std::string& aAttrName, bool& aTarget );
    template bool GetXMLattribute<false>( const pugi::xml_node& aNode , const std::string& aAttrName, bool& aTarget );
  }
}
