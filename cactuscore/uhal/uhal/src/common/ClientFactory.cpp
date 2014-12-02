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

#include "uhal/ProtocolUDP.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolIPbus.hpp"
#include "uhal/ProtocolControlHub.hpp"

#include "uhal/ClientFactory.hpp"


namespace uhal
{

  ClientFactory& ClientFactory::getInstance()
  {
    if ( mInstance == NULL )
    {
      mInstance = new ClientFactory();
      // ---------------------------------------------------------------------
      mInstance->add< UDP< IPbus< 1 , 3 , 350 > > > ( "ipbusudp-1.3" , "Direct access to hardware via UDP, using IPbus version 1.3" );
      mInstance->add< UDP< IPbus< 2 , 0 , 350 > > > ( "ipbusudp-2.0" , "Direct access to hardware via UDP, using IPbus version 2.0" );
      // ---------------------------------------------------------------------
      mInstance->add< TCP< IPbus< 1 , 3 , 350 > , 1 > > ( "ipbustcp-1.3" , "Direct access to hardware via TCP, using IPbus version 1.3" );
      mInstance->add< TCP< IPbus< 2 , 0 , 350 > , 1 > > ( "ipbustcp-2.0" , "Direct access to hardware via TCP, using IPbus version 2.0" );
      // ---------------------------------------------------------------------
      mInstance->add< TCP< ControlHub < IPbus< 1 , 3 , 350 > > , 3 > > ( "chtcp-1.3", "Hardware access via the Control Hub, using IPbus version 1.3" );
      mInstance->add< TCP< ControlHub < IPbus< 2 , 0 , 350 > > , 3 > > ( "chtcp-2.0", "Hardware access via the Control Hub, using IPbus version 2.0" );
      // ---------------------------------------------------------------------
    }

    return *mInstance;
  }


  ClientFactory* ClientFactory::mInstance = NULL;


  ClientFactory::ClientFactory()
  {
  }


  ClientFactory::~ClientFactory()
  {
  }


  boost::shared_ptr<ClientInterface> ClientFactory::getClient ( const std::string& aId , const std::string& aUri )
  {
    URI lUri;

    try
    {
      grammars::URIGrammar lGrammar;
      std::string::const_iterator lBegin ( aUri.begin() );
      std::string::const_iterator lEnd ( aUri.end() );
      boost::spirit::qi::phrase_parse ( lBegin , lEnd , lGrammar , boost::spirit::ascii::space , lUri );
    }
    catch ( const std::exception& aExc )
    {
      exception::FailedToParseURI lExc;
      log ( lExc , "Failed to parse device URI " , Quote ( aUri ) );
      throw lExc;
    }

    log ( Info() , "URI " , Quote ( aUri ) , " parsed as:\n" , lUri );
    boost::unordered_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( lUri.mProtocol );

    if ( lIt == mCreators.end() )
    {
      std::stringstream lStr;

      for ( std::map< std::string , std::string >::const_iterator lIt = mProductDescriptions.begin() ; lIt != mProductDescriptions.end() ; ++lIt )
      {
        lStr << "\n > " << lIt->first << "\t: " << lIt->second;
      }

      exception::ProtocolDoesNotExist lExc;
      log ( lExc , "Protocol " , Quote ( lUri.mProtocol ) , " does not exists in map of creators. Options are:" , lStr.str() );
      throw lExc;
    }

    return lIt->second->create ( aId , lUri );
  }


}

