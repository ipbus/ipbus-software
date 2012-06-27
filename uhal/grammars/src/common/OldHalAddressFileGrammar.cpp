// #include "uhal/grammars/OldHalAddressFileGrammar.hpp"


// namespace grammars
// {

	// OldHalSkipParser::OldHalSkipParser() :
		// OldHalSkipParser::base_type ( skip )
	// {
		// using namespace boost::spirit;
		// skip = ascii::blank | ( qi::lit ( "*" ) >> * ( qi::char_ - qi::eol ) >> - ( qi::eol ) );
	// }


	// OldHalEntryGrammar::OldHalEntryGrammar() :
		// OldHalEntryGrammar::base_type ( OldHalEntryVector )
	// {
		// using namespace boost::spirit;
		// OldHalEntryVector = *OldHalEntry;
		// OldHalEntry = key >> AM >> width >> address >> mask >> read >> write >> description;
		// //BChannelLongExt		39			2			000000c2    00000001    0    		1    		0= int, 1=ext regs
		// key = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
		// AM = qi::lexeme[ uint_ >> + ( ascii::blank ) ];
		// width = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
		// address = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
		// mask = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
		// read = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
		// write = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
		// description = qi::lexeme[ * ( qi::char_-qi::eol ) >> qi::eol ];
	// }

// }

