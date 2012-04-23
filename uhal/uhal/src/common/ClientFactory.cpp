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
				// ---------------------------------------------------------------------
				mInstance->add< uhal::IPBusUDPClient<IPbus_1_3> > ( "ipbusudp" );
				mInstance->add< uhal::IPBusUDPClient<IPbus_1_2> > ( "ipbusudp-1.2" );
				mInstance->add< uhal::IPBusUDPClient<IPbus_1_3> > ( "ipbusudp-1.3" );
				mInstance->add< uhal::IPBusUDPClient<IPbus_1_4> > ( "ipbusudp-1.4" );
				mInstance->add< uhal::IPBusUDPClient<IPbus_2_0> > ( "ipbusudp-2.0" );
				// ---------------------------------------------------------------------
				mInstance->add< uhal::IPBusTCPClient<IPbus_1_3> > ( "ipbustcp" );
				mInstance->add< uhal::IPBusTCPClient<IPbus_1_3> > ( "ipbustcp-1.3" );
				mInstance->add< uhal::IPBusTCPClient<IPbus_1_4> > ( "ipbustcp-1.4" );
				mInstance->add< uhal::IPBusTCPClient<IPbus_2_0> > ( "ipbustcp-2.0" );
				// ---------------------------------------------------------------------
				mInstance->add< uhal::ControlHubClient<IPbus_1_3> > ( "chtcp" );
				mInstance->add< uhal::ControlHubClient<IPbus_1_3> > ( "chtcp-1.3" );
				mInstance->add< uhal::ControlHubClient<IPbus_1_4> > ( "chtcp-1.4" );
				mInstance->add< uhal::ControlHubClient<IPbus_2_0> > ( "chtcp-2.0" );
				// ---------------------------------------------------------------------
				// mInstance->add< uhal::DummyClient >( "dummy" );
				// ---------------------------------------------------------------------
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
			pantheios::log_NOTICE ( "URI \"" , aUri , "\" parsed as:\n" , lazy_stream_inserter ( lUri ) );
			std::hash_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( lUri.mProtocol );

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

