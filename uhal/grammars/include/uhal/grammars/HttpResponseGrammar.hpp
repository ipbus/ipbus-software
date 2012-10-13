#ifndef _uhal_HttpResponseGrammar_hpp_
#define _uhal_HttpResponseGrammar_hpp_

//#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
//#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_grammar.hpp>


#include <vector>
#include <string>
#include <iostream>


namespace uhal
{

  //! Struct to store an http response received from a server when parsed by boost spirit
  struct HttpResponseType
  {
    //! the http transport method
    std::string method;
    //! the http version number
    double version;
    //! the response status
    int status;
    //! the response status string
    std::string status_string;
    //! parsed headers
    std::string headers;
    //! parsed message content
    std::vector< uint8_t > content;
  };
}

// std::ostream& operator<< ( std::ostream& aStream , const uhal::HttpResponseType& aHttpResponse );


// Call to BOOST_FUSION_ADAPT_STRUCT must be at global scope
//! A boost::fusion adaptive struct used by the boost::qi parser
BOOST_FUSION_ADAPT_STRUCT (
  uhal::HttpResponseType,
  ( std::string, method )
  ( double , version )
  ( int, status )
  ( std::string, status_string )
  ( std::string, headers )
  ( std::vector< uint8_t >, content )
);


namespace grammars
{
  //! A struct wrapping a set of rules as a grammar that can parse an HTTP response packet
  struct HttpResponseGrammar : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator , uhal::HttpResponseType() ,	boost::spirit::ascii::space_type >
  {
    //! Default Constructor where we will define the boost::qi rules relating the members
    HttpResponseGrammar();
    //! Boost spirit parsing rule for parsing the incoming http packet
    boost::spirit::qi::rule< std::vector<uint8_t>::iterator,	uhal::HttpResponseType(),								boost::spirit::ascii::space_type > response;
    //! Boost spirit parsing rule for parsing the incoming http packet
    boost::spirit::qi::rule< std::vector<uint8_t>::iterator, std::string(),												boost::spirit::ascii::space_type > nospace_sequence;
    //! Boost spirit parsing rule for parsing the incoming http packet
    boost::spirit::qi::rule< std::vector<uint8_t>::iterator,	std::string(),											boost::spirit::ascii::space_type > nonewline_sequence;
    //! Boost spirit parsing rule for parsing the incoming http packet
    boost::spirit::qi::rule< std::vector<uint8_t>::iterator,	std::string(),											boost::spirit::ascii::space_type > noblankline_sequence;
  };


}






#endif
