#ifndef _uhal_SemicolonDelimitedUriListGrammar_hpp_
#define _uhal_SemicolonDelimitedUriListGrammar_hpp_

#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>

#include <string>
#include <vector>


namespace BoostSpiritGrammars
{

/// The BOOST::SPIRIT grammar for parsing the Semicolon delimited URI list
	struct SemicolonDelimitedUriListGrammar : boost::spirit::qi::grammar< std::string::const_iterator , std::vector< std::pair<std::string, std::string> >() , boost::spirit::ascii::space_type > {
		SemicolonDelimitedUriListGrammar();
		//! Boost spirit parsing rule for parsing a Semicolon delimited URI list
		boost::spirit::qi::rule< std::string::const_iterator , std::vector< std::pair<std::string, std::string> > () ,	boost::spirit::ascii::space_type > data_pairs_vector;
		//! Boost spirit parsing rule for parsing a Semicolon delimited URI list
		boost::spirit::qi::rule< std::string::const_iterator , std::pair<std::string, std::string>() ,					boost::spirit::ascii::space_type > data_pairs;
		//! Boost spirit parsing rule for parsing a Semicolon delimited URI list
		boost::spirit::qi::rule< std::string::const_iterator , std::string() ,											boost::spirit::ascii::space_type > data_pairs_1;
		//! Boost spirit parsing rule for parsing a Semicolon delimited URI list
		boost::spirit::qi::rule< std::string::const_iterator , std::string() ,											boost::spirit::ascii::space_type > data_pairs_2;	
	};					
	
	
}	
	

	
	


#endif
