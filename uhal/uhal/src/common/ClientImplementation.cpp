#include "uhal/ClientImplementation.hpp"

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_eps.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/asio.hpp>

#include <vector>
#include <string>

namespace uhal
{

	std::pair< uint32_t , uint16_t > ExtractTargetID ( const URI& aUri )
	{
		NameValuePairVectorType::const_iterator lIt = aUri.mArguments.begin();

		for ( ; lIt != aUri.mArguments.end() ; ++lIt )
		{
			if ( lIt->first == "target" )
			{
				break;
			}
		}

		if ( lIt == aUri.mArguments.end() )
		{
			log ( Error() , "This function expects arguments of the form " , Quote ( "target=192.168.200.200:50001" ) ,". It appears that this is missing." );
			XMLfileMissingRequiredParameters().throwFrom ( ThisLocation() );
		}

		/*
				IPaddr lIP;

				try
				{
					boost::spirit::qi::phrase_parse (	lIt->second.begin() ,
														lIt->second.end() ,
														( boost::spirit::qi::eps >
															boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
															boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
															boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
															boost::spirit::qi::uint_ > boost::spirit::qi::lit ( ":" ) >
															boost::spirit::qi::uint_ ) ,
														boost::spirit::ascii::space ,
														lIP
													);
				}
				catch ( const std::exception& aExc )
				{
					log ( Error() , "Expected a string of the form " , Quote ( "aaa.bbb.ccc.ddd:eeeee" ) , " but received " , Quote ( lIt->second ) , "." );
					StdException ( aExc ).throwFrom ( ThisLocation() );
				}

				uint32_t lIPaddress = ( lIPAddr[0] <<24 ) | ( lIPAddr[1] <<16 ) | ( lIPAddr[2] <<8 ) | ( lIPAddr[3] );
				log ( Info() , "Converted IP address string " ,  Quote ( lIt->second ) ,
					  " to " , Integer ( lIPAddr[0] ) , "." , Integer ( lIPAddr[1] ) , "." , Integer ( lIPAddr[2] ) , "." , Integer ( lIPAddr[3] ) , ":" , Integer ( lIP.mPort ) ,
					  " and converted this to IP " , Integer ( lIPaddress, IntFmt< hex , fixed >() ) , ", port " , Integer ( lIP.mPort, IntFmt< hex , fixed >() )	);
				return std::make_pair ( lIPaddress , lIP.mPort );
			}
		*/
		std::pair< std::string , std::string > lIP;

		try
		{
			boost::spirit::qi::phrase_parse (	lIt->second.begin() ,
												lIt->second.end() ,
												( boost::spirit::qi::eps >
												  * ( boost::spirit::qi::char_ - boost::spirit::qi::lit ( ":" ) ) >
												  boost::spirit::qi::lit ( ":" ) >
												  *boost::spirit::qi::char_ ) ,
												boost::spirit::ascii::space ,
												lIP
											);
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Expected a string of the form " , Quote ( "hostIP:port" ) , " or " , Quote ( "hostname:port" ) , " but received " , Quote ( lIt->second ) , "." );
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}

		std::string lAddr;
		uint16_t lPort;

		try
		{
			boost::asio::io_service lService;
			boost::asio::ip::udp::endpoint lEndpoint (
				*boost::asio::ip::udp::resolver::iterator (
					boost::asio::ip::udp::resolver ( lService ).resolve (
						boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , lIP.first , lIP.second )
					)
				)
			);
			lAddr = lEndpoint.address().to_string();
			lPort = lEndpoint.port();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Look up failed for hostname=" , lIP.first , ", port=" , lIP.second );
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}

		std::vector< uint32_t > lIPAddr;

		try
		{
			boost::spirit::qi::phrase_parse (	lAddr.begin() ,
												lAddr.end() ,
												( boost::spirit::qi::eps >
												  boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
												  boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
												  boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
												  boost::spirit::qi::uint_ ),
												boost::spirit::ascii::space ,
												lIPAddr
											);
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Boost::ASIO returned address " , Quote ( lAddr ) , " which could not be parsed as " , Quote ( "aaa.bbb.ccc.ddd" ) );
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}

		uint32_t lIPaddress = ( lIPAddr[0] <<24 ) | ( lIPAddr[1] <<16 ) | ( lIPAddr[2] <<8 ) | ( lIPAddr[3] );
		log ( Info() , "Converted IP address string " ,  Quote ( lIt->second ) , " to " ,
			  Integer ( lIPAddr[0] ) , "." , Integer ( lIPAddr[1] ) , "." , Integer ( lIPAddr[2] ) , "." , Integer ( lIPAddr[3] ) , ":" , Integer ( lPort ) ,
			  " and converted this to IP " , Integer ( lIPaddress, IntFmt< hex , fixed >() ) , ", port " , Integer ( lPort, IntFmt< hex , fixed >() ) );
		return std::make_pair ( lIPaddress , lPort );
	}
}


