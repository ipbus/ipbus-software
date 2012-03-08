#include "uhal/BoostSpiritGrammars.hpp"


namespace uhal
{
	SemicolonDelimitedUriListGrammar::SemicolonDelimitedUriListGrammar() :
		SemicolonDelimitedUriListGrammar::base_type( data_pairs_vector )
	{
		using namespace boost::spirit;
		data_pairs_vector = *data_pairs;
		data_pairs = data_pairs_1 > data_pairs_2;
		data_pairs_1 = +(qi::char_ - "://") > "://";
		data_pairs_2 = *(qi::char_ - qi::lit(";")) >> -(qi::lit(";"));
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
}

