#include "BoostSpiritGrammars/BoostSpiritGrammars.hpp"


namespace uhal
{
	SemicolonDelimitedUriListGrammar::SemicolonDelimitedUriListGrammar() :
		SemicolonDelimitedUriListGrammar::base_type( data_pairs_vector )
	{
		using namespace boost::spirit;
		data_pairs_vector = *data_pairs;
		data_pairs = data_pairs_1 > data_pairs_2;
		data_pairs_1 = *qi::lit(";") >> +(qi::char_ - "://") > "://";
		data_pairs_2 = *( qi::char_ - qi::lit(";")) >> *qi::lit(";");
	}
	
	
	URLGrammar::URLGrammar() :
		URLGrammar::base_type( data_pair )
	{
		using namespace boost::spirit;
		data_pair = data_pair_1 > data_pair_2;
		data_pair_1 = +(qi::char_ - "/") > "/";
		data_pair_2 = +(qi::char_);
	}	
	
	
	HttpResponseGrammar::HttpResponseGrammar() :
		HttpResponseGrammar::base_type( response )
	{
		using namespace boost::spirit;
		response = +(qi::char_ - qi::lit("/") ) >> qi::lit("/") >> double_ >> int_ >> nonewline_sequence >> noblankline_sequence >> qi::lexeme[ *(qi::char_) ] ;
		nospace_sequence = qi::lexeme[ +(qi::char_ - " ") > " " ];
		nonewline_sequence = qi::lexeme[ +(qi::char_ - "\r\n") >> -qi::lit("\r\n") ];		
		noblankline_sequence = qi::lexeme[ +(qi::char_ - "\r\n\r\n") >> -qi::lit("\r\n\r\n") ];		
	}		
	
	
	OldHalSkipParser::OldHalSkipParser() : 
		OldHalSkipParser::base_type(skip)
	{ 
		using namespace boost::spirit;
		skip = ascii::blank | (qi::lit("*") >> *(qi::char_ - qi::eol) >> -(qi::eol) ); 
	}
	
	
	OldHalEntryGrammar::OldHalEntryGrammar() :
		OldHalEntryGrammar::base_type( OldHalEntryVector )
	{
		using namespace boost::spirit;
		OldHalEntryVector = *OldHalEntry;
		OldHalEntry = key >> AM >> width >> address >> mask >> read >> write >> description;
						//BChannelLongExt		39			2			000000c2    00000001    0    		1    		0= int, 1=ext regs

		key = qi::lexeme[ +(qi::char_-ascii::blank) ];
		AM = qi::lexeme[ uint_ >> +(ascii::blank) ];       
		width = qi::lexeme[ +(qi::char_-ascii::blank) ];       
		address = qi::lexeme[ +(qi::char_-ascii::blank) ];        
		mask = qi::lexeme[ +(qi::char_-ascii::blank) ];         
		read = qi::lexeme[ +(qi::char_-ascii::blank) ];    
		write = qi::lexeme[ +(qi::char_-ascii::blank) ];      
		description = qi::lexeme[ *(qi::char_-qi::eol) >> qi::eol ];   									
	}		
	
}

