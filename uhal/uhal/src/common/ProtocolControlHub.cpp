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

#include "uhal/ProtocolControlHub.hpp"


#include <mutex>
#include <string>
#include <vector>

#include <boost/asio.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_eps.hpp>

#include "uhal/Buffers.hpp"
#include "uhal/ProtocolIPbus.hpp"


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


  template < typename InnerProtocol >
  ControlHub< InnerProtocol >::ControlHub ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mDeviceIPaddress ( 0 ),
    mDevicePort ( 0 )
  {
    std::pair< uint32_t , uint16_t > lPair ( ExtractTargetID ( aUri ) );
    mDeviceIPaddress = htonl ( lPair.first );
    mDevicePort = htons ( lPair.second );
  }


  template < typename InnerProtocol >
  ControlHub< InnerProtocol >::~ControlHub()
  {
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::preamble ( std::shared_ptr< Buffers > aBuffers )
  {
    // -------------------------------------------------------------------------------------------------------------
    // 8 bytes form the preamble:
    // Device IP address (4 bytes)
    // Device Port number (2 bytes)
    // Word-count (2 bytes) will be updated before transmission in predispatch
    // -------------------------------------------------------------------------------------------------------------
    // 12 bytes form the preamble reply:
    // Chunk Byte-count (4 bytes)
    // Device IP address (4 bytes)
    // Device Port number (2 bytes)
    // Error code (2 bytes)
    // -------------------------------------------------------------------------------------------------------------
    {
      std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.push_back ( tpreamble () );
      tpreamble* lPreambles = & mPreambles.back();
      //     lPreambles->mSendByteCountPtr = ( uint32_t* ) ( aBuffers->send ( ( uint32_t ) ( 0 ) ) );
      aBuffers->send ( mDeviceIPaddress );
      aBuffers->send ( mDevicePort );
      lPreambles->mSendWordCountPtr = ( uint16_t* ) ( aBuffers->send ( ( uint16_t ) ( 0 ) ) );
      //     aBuffers->receive ( lPreambles->mReplyTotalByteCounter );
      aBuffers->receive ( lPreambles->mReplyChunkByteCounter );
      aBuffers->receive ( lPreambles->mReplyDeviceIPaddress );
      aBuffers->receive ( lPreambles->mReplyDevicePort );
      aBuffers->receive ( lPreambles->mReplyErrorCode );
    }
    InnerProtocol::preamble ( aBuffers );
  }



  template < typename InnerProtocol >
  uint32_t ControlHub< InnerProtocol >::getPreambleSize()
  {
    return InnerProtocol::getPreambleSize() + 2;
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::predispatch ( std::shared_ptr< Buffers > aBuffers )
  {
    InnerProtocol::predispatch ( aBuffers );
    std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
    tpreamble& lPreambles = mPreambles.back();
    uint32_t lByteCount ( aBuffers->sendCounter() );
    * ( lPreambles.mSendWordCountPtr ) = htons ( ( lByteCount-8 ) >>2 );
  }


  template < typename InnerProtocol >
  exception::exception* ControlHub< InnerProtocol >::validate ( uint8_t* aSendBufferStart ,
      uint8_t* aSendBufferEnd ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt )
  {
    aReplyStartIt++;
    uint32_t lReplyIPaddress ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) );

    if ( lReplyIPaddress != mDeviceIPaddress )
    {
      uhal::exception::ControlHubReturnedWrongAddress* lExc = new uhal::exception::ControlHubReturnedWrongAddress();
      log ( *lExc , "Returned IP address " , Integer ( lReplyIPaddress , IntFmt< hex , fixed >() ) ,
            " does not match that sent " , Integer ( mDeviceIPaddress, IntFmt< hex , fixed >() ) ,
            " for device with URI: " , this->uri() );
      std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();
      return lExc;
    }

    aReplyStartIt++;
    uint16_t lReplyPort ( * ( ( uint16_t* ) ( aReplyStartIt->first ) ) );

    if ( lReplyPort != mDevicePort )
    {
      uhal::exception::ControlHubReturnedWrongAddress* lExc = new uhal::exception::ControlHubReturnedWrongAddress();
      log ( *lExc , "Returned Port number " , Integer ( lReplyPort ) ,
            " does not match that sent " , Integer ( mDevicePort ) ,
            " for device with URI: " , this->uri() );
      std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();
      return lExc;
    }

    aReplyStartIt++;
    uint16_t lErrorCode ( ntohs ( * ( ( uint16_t* ) ( aReplyStartIt->first ) ) ) );

    if ( lErrorCode != 0 )
    {
      std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();

      if ( lErrorCode == 1 || lErrorCode == 3 || lErrorCode == 4 )
      {
        uhal::exception::ControlHubTargetTimeout* lExc = new uhal::exception::ControlHubTargetTimeout();
        log ( *lExc , "The ControlHub did not receive any response from the target with URI ", Quote(this->uri()) );
        log ( *lExc , "ControlHub returned error code ", Integer ( lErrorCode ), " = '", TranslatedFmt<uint16_t>(lErrorCode, translateErrorCode), "'");
        return lExc ;
      }

      uhal::exception::ControlHubErrorCodeSet* lExc = new uhal::exception::ControlHubErrorCodeSet();
      log ( *lExc , "The ControlHub returned error code " , Integer ( lErrorCode, IntFmt< hex , fixed >() ),
            " = ", TranslatedFmt<uint16_t>(lErrorCode, translateErrorCode),
            " for target with URI " , Quote(this->uri()) );
      return lExc;
    }

    {
      std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();
    }
    return InnerProtocol::validate ( ( aSendBufferStart+=8 ) , aSendBufferEnd , ( ++aReplyStartIt ) , aReplyEndIt );
  }


  template < typename InnerProtocol >
  uint32_t ControlHub< InnerProtocol >::getMaxNumberOfBuffers()
  {
    return 60;
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::dispatchExceptionHandler()
  {
    {
      std::lock_guard<std::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.clear();
    }
    InnerProtocol::dispatchExceptionHandler();
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::translateErrorCode(std::ostream& aStream, const uint16_t& aErrorCode)
  {
    switch (aErrorCode) {
      case 0:
        aStream << "success";
        break;
      case 1:
        aStream << "no reply to control packet";
        break;
      case 2:
        aStream << "internal timeout within ControlHub";
        break;
      case 3:
        aStream << "no reply to status packet";
        break;
      case 4:
        aStream << "no reply to resend request";
        break;
      case 5:
        aStream << "malformed status packet received";
        break;
      case 6:
        aStream << "request uses incorrect protocol version";
        break;
      case 7:
        aStream << "device is not in the ControlHub's allowlist";
        break;
      default:
        aStream << "UNKNOWN";
    }
  }


  template class ControlHub< IPbus<1, 3> >;
  template class ControlHub< IPbus<2, 0> >;
}


