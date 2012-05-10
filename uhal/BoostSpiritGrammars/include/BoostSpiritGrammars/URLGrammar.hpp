#ifndef _uhal_URIGrammar_hpp_
#define _uhal_URIGrammar_hpp_

#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>

#include <string>
#include <vector>


namespace BoostSpiritGrammars
{

	//! The BOOST::SPIRIT grammar for parsing a URL
	struct URIGrammarShort : boost::spirit::qi::grammar< std::string::const_iterator , std::pair<std::string, std::string>() , boost::spirit::ascii::space_type >
	{
		URIGrammarShort();
		//! Boost spirit parsing rule for parsing a URL
		boost::spirit::qi::rule< std::string::const_iterator , std::pair<std::string, std::string>() ,					boost::spirit::ascii::space_type > data_pair;
		//! Boost spirit parsing rule for parsing a URL
		boost::spirit::qi::rule< std::string::const_iterator , std::string() ,											boost::spirit::ascii::space_type > data_pair_1;
		//! Boost spirit parsing rule for parsing a URL
		boost::spirit::qi::rule< std::string::const_iterator , std::string() ,											boost::spirit::ascii::space_type > data_pair_2;
	};
}



namespace uhal
{
	typedef std::vector< std::pair<std::string, std::string> > NameValuePairVectorType;

	//! Struct to store a URI when parsed by boost spirit
	struct URI
	{
		std::string mProtocol;
		std::string mHostname;
		std::string mPort;
		std::string mPath;
		std::string mExtension;
		NameValuePairVectorType mArguments;
	};
}

std::ostream& operator<< ( std::ostream& aStream , const uhal::URI& aURI );



BOOST_FUSION_ADAPT_STRUCT (
	uhal::URI,
	( std::string , mProtocol )
	( std::string , mHostname )
	( std::string , mPort )
	( std::string , mPath )
	( std::string , mExtension )
	( uhal::NameValuePairVectorType, mArguments )
);


namespace BoostSpiritGrammars
{
	struct URIGrammar : boost::spirit::qi::grammar<std::string::const_iterator, uhal::URI(), boost::spirit::ascii::space_type>
	{
		URIGrammar();
		boost::spirit::qi::rule< std::string::const_iterator,	uhal::URI(), 											boost::spirit::ascii::space_type > start;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > protocol;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > hostname;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > port;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > path;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > extension;
		boost::spirit::qi::rule< std::string::const_iterator,	std::vector< std::pair<std::string, std::string> > (),	boost::spirit::ascii::space_type > data_pairs_vector; //NameValuePairVectorType
		boost::spirit::qi::rule< std::string::const_iterator,	std::pair<std::string, std::string>(),					boost::spirit::ascii::space_type > data_pairs;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_1;
		boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_2;
	};
}

#endif
