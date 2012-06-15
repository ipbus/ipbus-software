#include "BoostSpiritGrammars/URLGrammar.hpp"


namespace BoostSpiritGrammars
{

	URIGrammarShort::URIGrammarShort() :
		URIGrammarShort::base_type ( data_pair )
	{
		using namespace boost::spirit;
		data_pair = data_pair_1 >> data_pair_2;
		data_pair_1 = + ( qi::char_ - "/" ) >> -qi::lit ( "/" );
		data_pair_2 = + ( qi::char_ );
	}
}


std::ostream& operator<< ( std::ostream& aStream , const uhal::URI& aURI )
{
	aStream << " > protocol : " << aURI.mProtocol << "\n";
	aStream << " > hostname : " << aURI.mHostname << "\n";
	aStream << " > port : " << aURI.mPort << "\n";
	aStream << " > path : " << aURI.mPath << "\n";
	aStream << " > extension : " << aURI.mExtension << "\n";
	aStream << " > arguments :\n";

	for ( uhal::NameValuePairVectorType::const_iterator lIt = aURI.mArguments.begin() ; lIt != aURI.mArguments.end() ; ++lIt )
	{
		aStream << "   > " << lIt->first << " = " << lIt->second << "\n";
	}

	aStream << std::flush;
	return aStream;
}


namespace BoostSpiritGrammars
{
	URIGrammar::URIGrammar() :
		URIGrammar::base_type ( start )
	{
		using namespace boost::spirit;
		start = protocol > hostname > port > - ( path ) > - ( extension ) > - ( data_pairs_vector );
		protocol = + ( qi::char_ - qi::lit ( ":" ) ) > qi::lit ( "://" );
		hostname = + ( qi::char_ - qi::lit ( ":" ) ) > qi::lit ( ":" );
		port 	 = + ( qi::char_ - ascii::punct ) ;
		path 				= qi::lit ( "/" ) > + ( qi::char_ - qi::lit ( "." ) - qi::lit ( "?" ) );
		extension 			= qi::lit ( "." ) > + ( qi::char_ - qi::lit ( "?" ) ) ;
		data_pairs_vector 	= qi::lit ( "?" ) > *data_pairs;
		data_pairs = data_pairs_1 > data_pairs_2;
		data_pairs_1 = + ( qi::char_ - qi::lit ( "=" ) ) > qi::lit ( "=" );
		data_pairs_2 = * ( qi::char_ - qi::lit ( "&" ) ) >> - ( qi::lit ( "&" ) );
	}

}

