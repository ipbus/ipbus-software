#include "uhal/ClientImplementation.hpp"



namespace uhal
{

	std::pair< uint32_t , uint16_t > ExtractTargetID ( const URI& aUri )
	{
		/*NameValuePairVectorType::const_iterator lIPstr = aUri.mArguments.end();
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
			log ( Error() , "This function expects arguments of the form IP=192.168.200.200&port=50001. One or both of these was missing" );
			log ( Error() , "Throwing at " , ThisLocation() );
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
		log ( Info() , "Converted IP address string \"" , lIPstr->second ,
			  "\" to " , Integer ( lIP.at ( 0 ) ) , "." , Integer ( lIP.at ( 1 ) ) , "." , Integer ( lIP.at ( 2 ) ) , "." , Integer ( lIP.at ( 3 ) ) ,
			  " and converted this to " , Integer ( lIPaddress, IntFmt< hex , fixed >() ) ,
			  ". Converted port string \"" , lPortStr->second ,
			  "\" to " , Integer ( lPort, IntFmt< hex , fixed >() )	);
		*/
		
		
		NameValuePairVectorType::const_iterator lIt = aUri.mArguments.begin();
		
		for (  ; lIt != aUri.mArguments.end() ; ++lIt )
		{
			if ( lIt->first == "target" )
			{
				break;
			}
		}
		
		if ( lIt == aUri.mArguments.end() )
		{
			log ( Error() , "This function expects arguments of the form \"target=192.168.200.200:50001\". It appears that this is missing." );
			log ( Error() , "Throwing at " , ThisLocation() );
			throw XMLfileMissingRequiredParameters();
		}
		
		IPaddr lIP;
		boost::spirit::qi::phrase_parse (	lIt->second.begin() ,
											lIt->second.end() ,
											( boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) > boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) > boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) > boost::spirit::qi::uint_ > boost::spirit::qi::lit ( ":" ) > boost::spirit::qi::uint_ ) ,
											boost::spirit::ascii::space ,
											lIP
										);

		uint32_t lIPaddress = ( lIP.mIP1 <<24 ) | ( lIP.mIP2 <<16 ) | ( lIP.mIP3 <<8 ) | ( lIP.mIP4 );
		log ( Info() , "Converted IP address string \"" , lIt->second ,
			  "\" to " , Integer ( lIP.mIP1 ) , "." , Integer ( lIP.mIP2 ) , "." , Integer ( lIP.mIP3 ) , "." , Integer ( lIP.mIP4 ) , ":" , Integer ( lIP.mPort ) ,
			  " and converted this to IP " , Integer ( lIPaddress, IntFmt< hex , fixed >() ) , ", port " , Integer ( lIP.mPort, IntFmt< hex , fixed >() )	);								
										
		return std::make_pair ( lIPaddress , lIP.mPort );
	}

}


