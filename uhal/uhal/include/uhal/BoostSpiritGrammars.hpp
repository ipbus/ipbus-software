#ifndef _uhal_BoostSpiritGrammars_hpp_
#define _uhal_BoostSpiritGrammars_hpp_

#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>

#include "uhal/UtilityTypes.hpp"

#include <string>
#include <vector>


namespace uhal
{

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	struct HttpResponseGrammar : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator , utilities::HttpResponseType() , boost::spirit::ascii::space_type > {
		HttpResponseGrammar();
		//! Boost spirit parsing rule for parsing the incoming http packet info
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator,	utilities::HttpResponseType(),						boost::spirit::ascii::space_type > response;
		//! Boost spirit parsing rule for parsing the incoming http packet
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, std::string(),											boost::spirit::ascii::space_type > nospace_sequence;
		//! Boost spirit parsing rule for parsing the incoming http packet
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator,	std::string(),										boost::spirit::ascii::space_type > nonewline_sequence;		
		//! Boost spirit parsing rule for parsing the incoming http packet
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator,	std::string(),										boost::spirit::ascii::space_type > noblankline_sequence;		
	};
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			


	struct OldHalSkipParser : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator > {
		OldHalSkipParser();
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator > skip; 
	}; 
			
			
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	struct OldHalEntryGrammar : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator , std::vector< utilities::OldHalEntryType >() , OldHalSkipParser > {
		OldHalEntryGrammar();
	
		//! Boost spirit parsing rule for parsing the incoming http packet info
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator,		std::vector< utilities::OldHalEntryType >(),						OldHalSkipParser > OldHalEntryVector;
		//! Boost spirit parsing rule for parsing the incoming http packet
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		utilities::OldHalEntryType(),										OldHalSkipParser > OldHalEntry;
		
		
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),														OldHalSkipParser > key;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		uint8_t(),															OldHalSkipParser > AM;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),															OldHalSkipParser > width;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),															OldHalSkipParser > address;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),															OldHalSkipParser > mask;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),																OldHalSkipParser > read;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),																OldHalSkipParser > write;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),														OldHalSkipParser > description;
		
		
		                                            
	};
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
				
	
}	
	

	
	


#endif
