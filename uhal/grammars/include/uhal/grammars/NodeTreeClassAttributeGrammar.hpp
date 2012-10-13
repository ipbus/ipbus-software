#ifndef _uhal_NodeTreeClassAttributeGrammar_hpp_
#define _uhal_NodeTreeClassAttributeGrammar_hpp_

#include <boost/fusion/adapted/std_pair.hpp>
//#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_grammar.hpp>

#include <string>
#include <vector>


namespace uhal
{
  //! boost::fusion requires us to typedef our template types, so typedef a container which can hold key/value pairs
  typedef std::vector< std::pair<std::string, std::string> > NameValuePairVectorType;

  //! Struct to store the name and member variables within a node class attribute when parsed by boost spirit
  struct NodeTreeClassAttribute
  {
    //! The name of the class
    std::string mClass;
    //! The member variable of the class stored as "name1=val1;name2=val2;name3=val3"
    NameValuePairVectorType mArguments;
  };
}


// Call to BOOST_FUSION_ADAPT_STRUCT must be at global scope
//! A boost::fusion adaptive struct used by the boost::qi parser
BOOST_FUSION_ADAPT_STRUCT (
  uhal::NodeTreeClassAttribute,
  ( std::string , mClass )
  ( uhal::NameValuePairVectorType, mArguments )
);


namespace grammars
{
  //! A struct wrapping a set of rules as a grammar that can parse a NodeTreeClassAttribute of the form "class;name1=val1;name2=val2;name3=val3"
  struct NodeTreeClassAttributeGrammar : boost::spirit::qi::grammar<std::string::const_iterator, uhal::NodeTreeClassAttribute(), boost::spirit::ascii::space_type>
  {
    //! Default Constructor where we will define the boost::qi rules relating the members
    NodeTreeClassAttributeGrammar();
    //! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    boost::spirit::qi::rule< std::string::const_iterator,	uhal::NodeTreeClassAttribute(), 						boost::spirit::ascii::space_type > start;
    //! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > classname;
    //! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    boost::spirit::qi::rule< std::string::const_iterator,	std::vector< std::pair<std::string, std::string> > (),	boost::spirit::ascii::space_type > data_pairs_vector; //NameValuePairVectorType
    //! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    boost::spirit::qi::rule< std::string::const_iterator,	std::pair<std::string, std::string>(),					boost::spirit::ascii::space_type > data_pairs;
    //! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_1;
    //! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_2;
  };
}

#endif
