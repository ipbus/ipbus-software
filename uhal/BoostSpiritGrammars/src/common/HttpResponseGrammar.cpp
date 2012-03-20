#include "BoostSpiritGrammars/HttpResponseGrammar.hpp"

namespace uhal
{

	std::ostream& operator<< ( std::ostream& aStream , const HttpResponseType& aHttpResponse )
	{
		aStream << " > method = " << aHttpResponse.method << "\n";
		aStream << " > version = " << aHttpResponse.version << "\n";
		aStream << " > status = " << aHttpResponse.status << "\n";
		aStream << " > status_string = " << aHttpResponse.status_string << "\n";
		aStream << " > NameValuePairs =\n" << aHttpResponse.headers << "\n";
		aStream << " > Content =\n";
		for ( std::vector<uint8_t>::const_iterator lIt = aHttpResponse.content.begin() ; lIt != aHttpResponse.content.end() ; ++lIt ){
			aStream << char(*lIt);
		}
		aStream << std::endl;
		return aStream;
	}
	
}	

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

