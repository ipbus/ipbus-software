/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

#ifndef _uhal_NodeTreeParametersGrammar_hpp_
#define _uhal_NodeTreeParametersGrammar_hpp_

#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/unordered_map.hpp>
#include <boost/spirit/include/qi_grammar.hpp>

#include <string>
#include <map>


/*namespace uhal*/
//{
  ////! boost::fusion requires us to typedef our template types, so typedef a container which can hold key/value pairs
  //typedef std::vector< std::pair<std::string, std::string> > NameValuePairVectorType;

  ////! Struct to store the name and member variables within a node class attribute when parsed by boost spirit
  //struct NodeTreeParameters
  //{
    ////! The member variable of the class stored as "name1=val1;name2=val2;name3=val3"
    //NameValuePairVectorType mParameters;
  //};
//}


// Call to BOOST_FUSION_ADAPT_STRUCT must be at global scope
//! A boost::fusion adaptive struct used by the boost::qi parser
//BOOST_FUSION_ADAPT_STRUCT (
  //uhal::NodeTreeParameters,
  //( uhal::NameValuePairVectorType, mParameters )
//);


//namespace grammars
//{
  ////! A struct wrapping a set of rules as a grammar that can parse a NodeTreeClassAttribute of the form "class;name1=val1;name2=val2;name3=val3"
  //struct NodeTreeParametersGrammar : boost::spirit::qi::grammar<std::string::const_iterator, uhal::NodeTreeParameters(), boost::spirit::ascii::space_type>
  //{
    ////! Default Constructor where we will define the boost::qi rules relating the members
    //NodeTreeParametersGrammar();
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	uhal::NodeTreeParameters(), 						    boost::spirit::ascii::space_type > start;
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::vector< std::pair<std::string, std::string> > (),	boost::spirit::ascii::space_type > data_pairs_vector; //NameValuePairVectorType
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::pair<std::string, std::string>(),					boost::spirit::ascii::space_type > data_pairs;
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_1;
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_2;
  //};
//}

namespace grammars
{
  ////! A struct wrapping a set of rules as a grammar that can parse a NodeTreeClassAttribute of the form "class;name1=val1;name2=val2;name3=val3"
  //struct NodeTreeParametersGrammar : boost::spirit::qi::grammar<std::string::const_iterator, uhal::NodeTreeParameters(), boost::spirit::ascii::space_type>
  //{
    ////! Default Constructor where we will define the boost::qi rules relating the members
    //NodeTreeParametersGrammar();
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	uhal::NodeTreeParameters(), 						    boost::spirit::ascii::space_type > start;
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::vector< std::pair<std::string, std::string> > (),	boost::spirit::ascii::space_type > data_pairs_vector; //NameValuePairVectorType
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::pair<std::string, std::string>(),					boost::spirit::ascii::space_type > data_pairs;
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_1;
    ////! Boost spirit parsing rule for parsing a NodeTreeClassAttribute
    //boost::spirit::qi::rule< std::string::const_iterator,	std::string(),											boost::spirit::ascii::space_type > data_pairs_2;
  //};
  struct NodeTreeParametersGrammar
  : boost::spirit::qi::grammar<std::string::const_iterator, boost::unordered_map<std::string, std::string>()>
  {
    NodeTreeParametersGrammar();
    boost::spirit::qi::rule<std::string::const_iterator, boost::unordered_map<std::string, std::string>()> query;
    boost::spirit::qi::rule<std::string::const_iterator, std::pair<std::string, std::string>()> pair;
    boost::spirit::qi::rule<std::string::const_iterator, std::string()> key, value;
};
}

#endif
