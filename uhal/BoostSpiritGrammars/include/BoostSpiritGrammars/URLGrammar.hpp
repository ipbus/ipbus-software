#ifndef _uhal_URLGrammar_hpp_
#define _uhal_URLGrammar_hpp_

#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>

#include <string>
#include <vector>


namespace BoostSpiritGrammars
{

/// The BOOST::SPIRIT grammar for parsing a URL
	struct URLGrammar : boost::spirit::qi::grammar< std::string::const_iterator , std::pair<std::string, std::string>() , boost::spirit::ascii::space_type > {
		URLGrammar();
		//! Boost spirit parsing rule for parsing a URL
		boost::spirit::qi::rule< std::string::const_iterator , std::pair<std::string, std::string>() ,					boost::spirit::ascii::space_type > data_pair;
		//! Boost spirit parsing rule for parsing a URL
		boost::spirit::qi::rule< std::string::const_iterator , std::string() ,											boost::spirit::ascii::space_type > data_pair_1;
		//! Boost spirit parsing rule for parsing a URL
		boost::spirit::qi::rule< std::string::const_iterator , std::string() ,											boost::spirit::ascii::space_type > data_pair_2;	
	};					

	
}	
	

	
	


#endif
