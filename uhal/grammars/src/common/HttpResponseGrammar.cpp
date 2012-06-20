#include "uhal/grammars/HttpResponseGrammar.hpp"

#include "uhal/log/log.hpp"

std::ostream& operator<< ( std::ostream& aStream , const uhal::HttpResponseType& aHttpResponse )
{
	aStream << " > method = " << aHttpResponse.method << "\n";
	aStream << " > version = " << aHttpResponse.version << "\n";
	aStream << " > status = " << aHttpResponse.status << "\n";
	aStream << " > status_string = " << aHttpResponse.status_string << "\n";
	aStream << " > NameValuePairs =\n" << aHttpResponse.headers << "\n";
	aStream << " > Content =\n";

	for ( std::vector<uint8_t>::const_iterator lIt = aHttpResponse.content.begin() ; lIt != aHttpResponse.content.end() ; ++lIt )
	{
		aStream << char ( *lIt );
	}

	aStream << std::endl;
	return aStream;
}


namespace uhal
{
	template < >
	void log_inserter< HttpResponseType > ( const HttpResponseType& aHttpResponse )
	{
		log_inserter ( " > method = " );
		log_inserter ( aHttpResponse.method );
		log_inserter ( "\n > version = " );
		log_inserter ( Real ( aHttpResponse.version ) );
		log_inserter ( "\n > status = " );
		log_inserter ( Integer ( aHttpResponse.status ) );
		log_inserter ( "\n > status_string = " );
		log_inserter ( aHttpResponse.status_string );
		log_inserter ( "\n > NameValuePairs =\n" );
		log_inserter ( aHttpResponse.headers );
		log_inserter ( "\n > Content =\n" );
		fwrite ( & ( aHttpResponse.content[0] ) , 1 , aHttpResponse.content.size() , log_configuration::getDestination() );
	}
}


namespace grammars
{

	HttpResponseGrammar::HttpResponseGrammar() :
		HttpResponseGrammar::base_type ( response )
	{
		using namespace boost::spirit;
		response = + ( qi::char_ - qi::lit ( "/" ) ) >> qi::lit ( "/" ) >> double_ >> int_ >> nonewline_sequence >> noblankline_sequence >> qi::lexeme[ * ( qi::char_ ) ] ;
		nospace_sequence = qi::lexeme[ + ( qi::char_ - " " ) > " " ];
		nonewline_sequence = qi::lexeme[ + ( qi::char_ - "\r\n" ) >> -qi::lit ( "\r\n" ) ];
		noblankline_sequence = qi::lexeme[ + ( qi::char_ - "\r\n\r\n" ) >> -qi::lit ( "\r\n\r\n" ) ];
	}


}

