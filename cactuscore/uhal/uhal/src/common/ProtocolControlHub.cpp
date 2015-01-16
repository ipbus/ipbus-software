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

#include "uhal/Utilities.hpp"

#include "uhal/ProtocolControlHub.hpp"

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_eps.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/asio.hpp>

#include <vector>
#include <string>


namespace uhal
{

  std::pair< uint32_t , uint16_t > ExtractTargetID ( const URI& aUri )
  {
    NameValuePairVectorType::const_iterator lIt = aUri.mArguments.begin();

    for ( ; lIt != aUri.mArguments.end() ; ++lIt )
    {
      if ( lIt->first == "target" )
      {
        break;
      }
    }

    if ( lIt == aUri.mArguments.end() )
    {
      exception::XMLfileMissingRequiredParameters lExc;
      log ( lExc , "Expected URI arguments of the form " , Quote ( "target=192.168.200.200:50001" ) ,". It appears that this is missing in URI " , aUri );
      throw lExc;
    }

    std::pair< std::string , std::string > lIP;

    try
    {
      boost::spirit::qi::phrase_parse (	lIt->second.begin() ,
                                        lIt->second.end(),
                                        ( boost::spirit::qi::eps >
                                          * ( boost::spirit::qi::char_ - boost::spirit::qi::lit ( ":" ) ) >
                                          boost::spirit::qi::lit ( ":" ) >
                                          *boost::spirit::qi::char_
                                        ) ,
                                        boost::spirit::ascii::space ,
                                        lIP
                                      );
    }
    catch ( const std::exception& aExc )
    {
      exception::ParsingTargetURLfailed lExc;
      log ( lExc , "Expected a string of the form " , Quote ( "hostIP:port" ) , " or " , Quote ( "hostname:port" ) , " but received " , Quote ( lIt->second ) , "." );
      throw lExc;
    }

    std::string lAddr;
    uint16_t lPort;

    try
    {
      boost::asio::io_service lService;
      boost::asio::ip::udp::endpoint lEndpoint (
        *boost::asio::ip::udp::resolver::iterator (
          boost::asio::ip::udp::resolver ( lService ).resolve (
            boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , lIP.first , lIP.second )
          )
        )
      );
      lAddr = lEndpoint.address().to_string();
      lPort = lEndpoint.port();
    }
    catch ( const std::exception& aExc )
    {
      exception::HostnameToIPlookupFailed lExc;
      log ( lExc , "Hostname to IP look up failed for hostname=" , lIP.first , ", port=" , lIP.second );
      log ( lExc , "ASIO threw exception with what returning: ", Quote ( aExc.what() ) );
      throw lExc;
    }

    std::vector< uint32_t > lIPAddr;

    try
    {
      boost::spirit::qi::phrase_parse (	lAddr.begin() ,
                                        lAddr.end() ,
                                        ( boost::spirit::qi::eps >
                                          boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
                                          boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
                                          boost::spirit::qi::uint_ > boost::spirit::qi::lit ( "." ) >
                                          boost::spirit::qi::uint_ ),
                                        boost::spirit::ascii::space ,
                                        lIPAddr
                                      );
    }
    catch ( const std::exception& aExc )
    {
      exception::ParsingTargetURLfailed lExc;
      log ( lExc , "Boost::ASIO returned address " , Quote ( lAddr ) , " for hostname " , Quote (lIP.first) ,  " which could not be parsed as " , Quote ( "aaa.bbb.ccc.ddd" ) );
      throw lExc;
    }

    uint32_t lIPaddress = ( lIPAddr[0] <<24 ) | ( lIPAddr[1] <<16 ) | ( lIPAddr[2] <<8 ) | ( lIPAddr[3] );
    log ( Info() , "Converted IP address string " ,  Quote ( lIt->second ) , " to " ,
          Integer ( lIPAddr[0] ) , "." , Integer ( lIPAddr[1] ) , "." , Integer ( lIPAddr[2] ) , "." , Integer ( lIPAddr[3] ) , ":" , Integer ( lPort ) ,
          " and converted this to IP " , Integer ( lIPaddress, IntFmt< hex , fixed >() ) , ", port " , Integer ( lPort, IntFmt< hex , fixed >() ) );
    return std::make_pair ( lIPaddress , lPort );
  }
}


