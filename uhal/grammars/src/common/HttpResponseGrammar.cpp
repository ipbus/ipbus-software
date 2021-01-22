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

#include "uhal/grammars/HttpResponseGrammar.hpp"


#include <boost/spirit/include/qi.hpp>


namespace uhal {

  std::ostream& operator<< ( std::ostream& aStr , const uhal::HttpResponseType& aHttpResponse )
  {
    aStr << " > method = " << aHttpResponse.method << "\n";
    aStr << " > version = " << aHttpResponse.version << "\n";
    aStr << " > status = " << aHttpResponse.status << "\n";
    aStr << " > status_string = " << aHttpResponse.status_string << "\n";
    aStr << " > NameValuePairs =\n" << aHttpResponse.headers << "\n";
    aStr << " > Content =\n";

    for ( auto& x: aHttpResponse.content )
    {
      aStr << char ( x );
    }

    aStr << std::endl;
    return aStr;
  }

}


namespace uhal
{
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
}

