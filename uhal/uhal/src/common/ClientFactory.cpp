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
				mInstance->add< uhal::IPBusUDPClient<IPbus_1_3> > ( "ipbusudp" , "Direct access to hardware via UDP, using the default IPbus version which is currently IPbus 1.3" );
				// mInstance->add< uhal::IPBusUDPClient<IPbus_1_2> > ( "ipbusudp-1.2" );
				mInstance->add< uhal::IPBusUDPClient<IPbus_1_3> > ( "ipbusudp-1.3" );
				// mInstance->add< uhal::IPBusUDPClient<IPbus_1_4> > ( "ipbusudp-1.4" );
				// mInstance->add< uhal::IPBusUDPClient<IPbus_2_0> > ( "ipbusudp-2.0" );
				// ---------------------------------------------------------------------
				mInstance->add< uhal::IPBusTCPClient<IPbus_1_3> > ( "ipbustcp" , "Direct access to hardware via TCP, using the default IPbus version which is currently IPbus 1.3" );
				mInstance->add< uhal::IPBusTCPClient<IPbus_1_3> > ( "ipbustcp-1.3" );
				// mInstance->add< uhal::IPBusTCPClient<IPbus_1_4> > ( "ipbustcp-1.4" );
				// mInstance->add< uhal::IPBusTCPClient<IPbus_2_0> > ( "ipbustcp-2.0" );
				// ---------------------------------------------------------------------
				/*
					mInstance->add< uhal::ControlHubClient<CHH_1 , IPbus_1_3> > ( "chtcp" , "Hardware access via the Control Hub, using Control Hub Protocol 1 (Multi target packets with 32-bit identifier) and the default IPbus version which is currently IPbus 1.3" );
					mInstance->add< uhal::ControlHubClient<CHH_1 , IPbus_1_2> > ( "chtcp-1.2" );
					mInstance->add< uhal::ControlHubClient<CHH_1 , IPbus_1_3> > ( "chtcp-1.3" );
					// mInstance->add< uhal::ControlHubClient<CHH_1 , IPbus_1_4> > ( "chtcp-1.4" );
					// mInstance->add< uhal::ControlHubClient<CHH_1 , IPbus_2_0> > ( "chtcp-2.0" );
					// ---------------------------------------------------------------------
					mInstance->add< uhal::ControlHubClient<CHH_2 , IPbus_1_3> > ( "chtcp2" , "Hardware access via the Control Hub, using Control Hub Protocol 2 (Multi target packets with 48-bit IP+port as identifier) and the default IPbus version which is currently IPbus 1.3" );
					mInstance->add< uhal::ControlHubClient<CHH_2 , IPbus_1_3> > ( "chtcp2-1.3" );
					// mInstance->add< uhal::ControlHubClient<CHH_2 , IPbus_1_4> > ( "chtcp2-1.4" );
					// mInstance->add< uhal::ControlHubClient<CHH_2 , IPbus_2_0> > ( "chtcp2-2.0" );
				*/
				// ---------------------------------------------------------------------
				mInstance->add< uhal::ControlHubClient<IPbus_1_3> > ( "chtcp" , "Hardware access via the Control Hub, using the default IPbus version which is currently IPbus 1.3" );
				mInstance->add< uhal::ControlHubClient<IPbus_1_3> > ( "chtcp-1.3" );
				// mInstance->add< uhal::ControlHubClient<CHH_3 , IPbus_1_4> > ( "chtcp2-1.4" );
				// mInstance->add< uhal::ControlHubClient<CHH_3 , IPbus_2_0> > ( "chtcp2-2.0" );
				// ---------------------------------------------------------------------
				// mInstance->add< uhal::DummyClient >( "dummy" );
				// ---------------------------------------------------------------------
			}

			return *mInstance;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
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
			grammars::URIGrammar lGrammar;
			boost::spirit::qi::phrase_parse ( aUri.begin() , aUri.end() , lGrammar , boost::spirit::ascii::space , lUri );
			log ( Info() , "URI \"" , aUri , "\" parsed as:\n" , lUri );
			std::hash_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( lUri.mProtocol );

			if ( lIt == mCreators.end() )
			{
				std::stringstream lStr;

				for ( std::map< std::string , std::string >::const_iterator lIt = mProductDescriptions.begin() ; lIt != mProductDescriptions.end() ; ++lIt )
				{
					lStr << "\n > " << lIt->first << "\t: " << lIt->second;
				}

				log ( Error() , "Protocol \"" , lUri.mProtocol , "\" does not exists in map of creators. Options are:" , lStr.str() );
				log ( Error() , "Throwing at " , ThisLocation() );
				throw ProtocolDoesNotExist();
			}

			return lIt->second->create ( aId , lUri );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


}

