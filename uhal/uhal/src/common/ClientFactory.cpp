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

#include "uhal/ClientFactory.hpp"

#include <algorithm>

#include <boost/spirit/include/qi.hpp>

#include "uhal/grammars/URIGrammar.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolIPbus.hpp"
#include "uhal/ProtocolControlHub.hpp"
#include "uhal/ProtocolMmap.hpp"
#include "uhal/ProtocolPCIe.hpp"



namespace uhal
{

  std::shared_ptr<ClientFactory> ClientFactory::mInstance;


  ClientFactory::ClientFactory()
  {
  }


  ClientFactory::~ClientFactory()
  {
  }


  ClientFactory& ClientFactory::getInstance()
  {
    if ( ! mInstance )
    {
      mInstance.reset(new ClientFactory());
      // ---------------------------------------------------------------------
      mInstance->add< UDP< IPbus< 1 , 3 > > > ( "ipbusudp-1.3" , "Direct access to hardware via UDP, using IPbus version 1.3", false );
      mInstance->add< UDP< IPbus< 2 , 0 > > > ( "ipbusudp-2.0" , "Direct access to hardware via UDP, using IPbus version 2.0", false );
      // ---------------------------------------------------------------------
      mInstance->add< TCP< IPbus< 1 , 3 > , 1 > > ( "ipbustcp-1.3" , "Direct access to hardware via TCP, using IPbus version 1.3", false );
      mInstance->add< TCP< IPbus< 2 , 0 > , 1 > > ( "ipbustcp-2.0" , "Direct access to hardware via TCP, using IPbus version 2.0", false );
      // ---------------------------------------------------------------------
      mInstance->add< TCP< ControlHub < IPbus< 1 , 3 > > , 3 > > ( "chtcp-1.3", "Hardware access via the Control Hub, using IPbus version 1.3", false );
      mInstance->add< TCP< ControlHub < IPbus< 2 , 0 > > , 3 > > ( "chtcp-2.0", "Hardware access via the Control Hub, using IPbus version 2.0", false );
      // ---------------------------------------------------------------------
      mInstance->add< PCIe > ( "ipbuspcie-2.0" , "Direct access to hardware via PCIe, using IPbus version 2.0", false );
      mInstance->add< Mmap > ( "ipbusmmap-2.0" , "Direct access to hardware via mmap, using IPbus version 2.0", false );
      // ---------------------------------------------------------------------

    }

    return *mInstance;
  }


  std::shared_ptr<ClientInterface> ClientFactory::getClient ( const std::string& aId , const std::string& aUri )
  {
    return getClient(aId, aUri, std::vector<std::string>());
  }


  std::shared_ptr<ClientInterface> ClientFactory::getClient ( const std::string& aId , const std::string& aUri, const std::vector<std::string>& aUserClientActivationList )
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
    const auto lIt = mClientMap.find ( lUri.mProtocol );

    if ( lIt == mClientMap.end() )
    {
      std::stringstream lStr;

      for (const auto& c: mClientMap)
        lStr << "\n > " << c.first << "\t: " << c.second.description;

      exception::ProtocolDoesNotExist lExc;
      log ( lExc , "Protocol " , Quote ( lUri.mProtocol ) , " does not exists in map of creators. Options are:" , lStr.str() );
      throw lExc;
    }
    else if (lIt->second.userDefined)
    {
      std::vector<std::string>::const_iterator lIt2 = std::find(aUserClientActivationList.begin(), aUserClientActivationList.end(), lUri.mProtocol);

      if (lIt2 == aUserClientActivationList.end()) {
        exception::ProtocolNotEnabled lExc;
        log ( lExc , "Protocol " , Quote ( lUri.mProtocol ) , " with user-defined client is not activated");
        throw lExc;
      }
    }

    return lIt->second.creator->create ( aId , lUri );
  }

}

