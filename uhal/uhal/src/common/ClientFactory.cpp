#include "uhal/ClientImplementation.hpp"

#include "uhal/ClientFactory.hpp"

namespace uhal
{

	ClientFactory& ClientFactory::getInstance()
	{
		try
		{
			if ( mInstance == NULL )
			{
				mInstance = new ClientFactory();
				mInstance->add< uhal::IPBusUDPClient > ( "ipbusudp" );
				mInstance->add< uhal::IPBusTCPClient > ( "ipbustcp" );
				mInstance->add< uhal::ControlHubClient > ( "chtcp" );
				// mInstance->add< uhal::DummyClient >( "dummy" );
			}

			return *mInstance;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}




	ClientFactory* ClientFactory::mInstance = NULL;

	ClientFactory::ClientFactory() {}

	ClientFactory::~ClientFactory() {}


	boost::shared_ptr<ClientInterface> ClientFactory::getClient ( const std::string& aId , const std::string& aUri )
	{
		try
		{
			URI lUri;
			BoostSpiritGrammars::URIGrammar lGrammar;
			boost::spirit::qi::phrase_parse ( aUri.begin() , aUri.end() , lGrammar , boost::spirit::ascii::space , lUri );
			pantheios::log_INFORMATIONAL ( "URI \"" , aUri , "\" parsed as:\n" , lazy_inserter ( lUri ) );
			std::hash_map< std::string , CreatorInterface* >::const_iterator lIt = mCreators.find ( lUri.mProtocol );

			if ( lIt == mCreators.end() )
			{
				pantheios::log_ERROR ( "Protocol \"" , lUri.mProtocol , "\" does not exists in map of creators." );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ProtocolDoesNotExist();
			}

			return lIt->second->create ( aId , lUri );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}

