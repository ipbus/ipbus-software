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


#include "uhal/grammars/URIGrammar.hpp"


#include <boost/spirit/include/qi.hpp>


namespace uhal
{
namespace grammars
{
  URIGrammar::URIGrammar() :
    URIGrammar::base_type ( start )
  {
    using namespace boost::spirit;
    start = protocol > hostname     > - ( port )   > - ( path ) > -( extension ) > - ( data_pairs_vector ); //EthernetURI | PCIeURI;

    // EthernetURI = protocol > hostname     > port         > - ( path ) > -( extension ) > - ( data_pairs_vector );
    // PCIeURI     = protocol > empty_string > empty_string >  path      > empty_string   > - ( data_pairs_vector );

    protocol = + ( qi::char_ - qi::lit ( ":" ) ) > qi::lit ( "://" );
    hostname = + ( qi::char_ - qi::lit ( ":" ) - qi::lit ( "?" ) ) ;
    port 	 = qi::lit ( ":" ) > + ( qi::char_ - ascii::punct ) ;
    path 				= qi::lit ( "/" ) > + ( qi::char_ - qi::lit ( "." ) - qi::lit ( "?" ) );
    extension 			= qi::lit ( "." ) > + ( qi::char_ - qi::lit ( "?" ) ) ;
    data_pairs_vector 	= qi::lit ( "?" ) > *data_pairs;
    data_pairs = data_pairs_1 > data_pairs_2;
    data_pairs_1 = + ( qi::char_ - qi::lit ( "=" ) ) > qi::lit ( "=" );
    data_pairs_2 = * ( qi::char_ - qi::lit ( "&" ) ) >> - ( qi::lit ( "&" ) );

    empty_string = boost::spirit::qi::as_string[ boost::spirit::eps ];
  }

}
}
