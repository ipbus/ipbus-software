#include "BoostSpiritGrammars/HttpResponseGrammar.hpp"


namespace BoostSpiritGrammars
{
	
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

