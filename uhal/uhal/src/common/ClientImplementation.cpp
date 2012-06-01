#include "uhal/ClientImplementation.hpp"



namespace uhal
{

	std::pair< uint32_t , uint16_t > ExtractTargetID ( const URI& aUri )
	{
		NameValuePairVectorType::const_iterator lIPstr = aUri.mArguments.end();
		NameValuePairVectorType::const_iterator lPortStr = aUri.mArguments.end();

		for ( NameValuePairVectorType::const_iterator lIt = aUri.mArguments.begin() ; lIt != aUri.mArguments.end() ; ++lIt )
		{
			if ( lIPstr == aUri.mArguments.end() )
			{
				if ( lIt->first == "IP" )
				{
					lIPstr=lIt;
				}
			}
			else if ( lPortStr == aUri.mArguments.end() )
			{
				if ( lIt->first == "port" )
				{
					lPortStr=lIt;
				}
			}
			else
			{
				break;
			}
		}

		if ( ( lIPstr == aUri.mArguments.end() ) || ( lPortStr == aUri.mArguments.end() ) )
		{
			pantheios::log_ERROR ( "This function expects arguments of the form IP=192.168.200.200&port=50001. One or both of these was missing" );
			pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
			throw XMLfileMissingRequiredParameters();
		}

		std::vector<uint32_t> lIP;
		boost::spirit::qi::phrase_parse (	lIPstr->second.begin() ,
											lIPstr->second.end() ,
											+ ( boost::spirit::qi::uint_ > -boost::spirit::qi::lit ( "." ) ) ,
											boost::spirit::ascii::space ,
											lIP
										);
		uint16_t lPort = boost::lexical_cast< uint16_t > ( lPortStr->second );
		uint32_t lIPaddress = ( lIP.at ( 0 ) <<24 ) | ( lIP.at ( 1 ) <<16 ) | ( lIP.at ( 2 ) <<8 ) | ( lIP.at ( 3 ) );
		pantheios::log_NOTICE ( "Converted IP address string \"" , lIPstr->second ,
								"\" to " , pantheios::integer ( lIP.at ( 0 ) ) , "." , pantheios::integer ( lIP.at ( 1 ) ) , "." , pantheios::integer ( lIP.at ( 2 ) ) , "." , pantheios::integer ( lIP.at ( 3 ) ) ,
								" and converted this to " , pantheios::integer ( lIPaddress  , pantheios::fmt::fullHex | 10 ) ,
								". Converted port string \"" , lPortStr->second ,
								"\" to " , pantheios::integer ( lPort  , pantheios::fmt::fullHex | 6 )	);
		return std::make_pair ( lIPaddress , lPort );
	}

}


