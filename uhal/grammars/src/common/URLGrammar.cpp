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

#include "uhal/grammars/URLGrammar.hpp"

#include <boost/spirit/include/qi.hpp>

#include "uhal/log/log.hpp"

std::ostream& operator<< ( std::ostream& aStream , const uhal::URI& aURI )
{
  aStream << " > protocol : " << aURI.mProtocol << "\n";
  aStream << " > hostname : " << aURI.mHostname << "\n";
  aStream << " > port : " << aURI.mPort << "\n";
  aStream << " > path : " << aURI.mPath << "\n";
  aStream << " > extension : " << aURI.mExtension << "\n";
  aStream << " > arguments :\n";

  for ( uhal::NameValuePairVectorType::const_iterator lIt = aURI.mArguments.begin() ; lIt != aURI.mArguments.end() ; ++lIt )
  {
    aStream << "   > " << lIt->first << " = " << lIt->second << "\n";
  }

  aStream << std::flush;
  return aStream;
}



namespace uhal
{
  /**
  	The log_inserter function to add a URI object to a log entry
  	@param aURI a URI object to format and print to log
  */
  template < >
  void log_inserter< URI > ( const URI& aURI )
  {
    log_inserter ( " > protocol : " );
    log_inserter ( aURI.mProtocol );
    log_inserter ( "\n > hostname : " );
    log_inserter ( aURI.mHostname );
    log_inserter ( "\n > port : " );
    log_inserter ( aURI.mPort );
    log_inserter ( "\n > path : " );
    log_inserter ( aURI.mPath );
    log_inserter ( "\n > extension : " );
    log_inserter ( aURI.mExtension );
    log_inserter ( "\n > arguments :\n" );

    for ( NameValuePairVectorType::const_iterator lIt = aURI.mArguments.begin() ; lIt != aURI.mArguments.end() ; ++lIt )
    {
      log_inserter ( "   > " );
      log_inserter ( lIt->first.data() );
      log_inserter ( " = " );
      log_inserter ( lIt->second.data() );
      log_inserter ( '\n' );
    }
  }
}



namespace grammars
{
  URIGrammar::URIGrammar() :
    URIGrammar::base_type ( start )
  {
    using namespace boost::spirit;
    start = protocol > hostname > port > - ( path ) > - ( extension ) > - ( data_pairs_vector );
    protocol = + ( qi::char_ - qi::lit ( ":" ) ) > qi::lit ( "://" );
    hostname = + ( qi::char_ - qi::lit ( ":" ) ) > qi::lit ( ":" );
    port 	 = + ( qi::char_ - ascii::punct ) ;
    path 				= qi::lit ( "/" ) > + ( qi::char_ - qi::lit ( "." ) - qi::lit ( "?" ) );
    extension 			= qi::lit ( "." ) > + ( qi::char_ - qi::lit ( "?" ) ) ;
    data_pairs_vector 	= qi::lit ( "?" ) > *data_pairs;
    data_pairs = data_pairs_1 > data_pairs_2;
    data_pairs_1 = + ( qi::char_ - qi::lit ( "=" ) ) > qi::lit ( "=" );
    data_pairs_2 = * ( qi::char_ - qi::lit ( "&" ) ) >> - ( qi::lit ( "&" ) );
  }

}

