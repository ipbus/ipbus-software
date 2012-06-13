#ifndef _uhal_OldHalAddressFileGrammar_hpp_
#define _uhal_OldHalAddressFileGrammar_hpp_

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>

#include <vector>
#include <string>
#include <iostream>



namespace uhal
{
	//! Struct to store an http response received from a server when parsed by boost spirit
	struct OldHalEntryType
	{
		//! the HAL key
		std::string key;
		//! the HAL AM
		uint8_t AM;
		//! the HAL width
		std::string width;
		//! the HAL address
		std::string address;
		//! the HAL mask
		std::string mask;
		//! the HAL read
		std::string read;
		//! the HAL write
		std::string write;
		//! the HAL description
		std::string description;
	};


	std::ostream& operator<< ( std::ostream& aStream , const OldHalEntryType& aOldHalEntry )
	{
		aStream << " > key = '" << aOldHalEntry.key << "'\n";
		aStream << " > AM = '" << aOldHalEntry.AM << "'\n";
		aStream << " > width = '" << aOldHalEntry.width << "'\n";
		aStream << " > address = '" << aOldHalEntry.address << "'\n";
		aStream << " > mask = '" << aOldHalEntry.mask << "'\n";
		//aStream << " > permissions = " << (aOldHalEntry.read?"r":"") << (aOldHalEntry.write?"w":"") << "\n";
		aStream << " > description = '" << aOldHalEntry.description << "'" << std::endl;
		return aStream;
	}

}

// Call to BOOST_FUSION_ADAPT_STRUCT must be at global scope
BOOST_FUSION_ADAPT_STRUCT (
	uhal::OldHalEntryType,
	( std::string , key )
	( uint8_t , AM )
	( std::string , width )
	( std::string , address )
	( std::string , mask )
	( std::string , read )
	( std::string , write )
	( std::string , description )
);



namespace grammars
{

	struct OldHalSkipParser : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator >
	{
		OldHalSkipParser();
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator > skip;
	};


	struct OldHalEntryGrammar : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator , std::vector< uhal::OldHalEntryType >() , OldHalSkipParser >
	{
		OldHalEntryGrammar();

		//! Boost spirit parsing rule for parsing the incoming http packet info
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator,		std::vector< uhal::OldHalEntryType >(),					OldHalSkipParser > OldHalEntryVector;
		//! Boost spirit parsing rule for parsing the incoming http packet
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		uhal::OldHalEntryType(),								OldHalSkipParser > OldHalEntry;


		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > key;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		uint8_t(),												OldHalSkipParser > AM;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > width;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > address;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > mask;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > read;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > write;
		boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > description;

	};


}






#endif
