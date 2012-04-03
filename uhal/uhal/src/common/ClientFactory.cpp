#include "uhal/ClientImplementation.hpp"

#include "uhal/ClientFactory.hpp"

namespace uhal
{

    ClientFactory& ClientFactory::getInstance()
    {
        if ( mInstance == NULL ) {
            mInstance = new ClientFactory();

            mInstance->add< uhal::IPBusUDPClient >( "ipbusudp" );
            mInstance->add< uhal::IPBusTCPClient >( "ipbustcp" );
            // mInstance->add< uhal::ControlHubClient >( "chtcp" );
            // mInstance->add< uhal::DummyClient >( "dummy" );
        }

        return *mInstance;
    }
	
	


    ClientFactory* ClientFactory::mInstance = NULL;

	ClientFactory::ClientFactory() {}
	
	ClientFactory::~ClientFactory() {}

	
	boost::shared_ptr<ClientInterface> ClientFactory::getClient ( const std::string& aId , const std::string& aUri )
	{
	
		URI lUri;
		try{
			BoostSpiritGrammars::URIGrammar lGrammar;
			boost::spirit::qi::phrase_parse( aUri.begin() , aUri.end() , lGrammar , boost::spirit::ascii::space , lUri );
		}catch( std::exception& aExc ){
			pantheios::log_ALERT( "Caught EXCEPTION \"" , aExc.what() , "\" in " , ThisLocation() );
		}
		
		pantheios::log_INFORMATIONAL ( "URI \"" , aUri , "\" parsed as:\n" , lazy_inserter(lUri) );			
	
		std::hash_map<std::string , CreatorInterface*>::const_iterator lIt = mCreators.find ( lUri.mProtocol );

		if ( lIt == mCreators.end() )
		{
			pantheios::log_ALERT ( "Protocol \"" , lUri.mProtocol , "\" does not exists in map of creators." );
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw ProtocolDoesNotExist();
		}

		return lIt->second->create ( aId , lUri );
	}	
	
	
}

