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

#include "uhal/Buffers.hpp"


namespace uhal
{


  template < typename InnerProtocol >
  ControlHub< InnerProtocol >::ControlHub ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mDeviceIPaddress ( 0 ),
    mDevicePort ( 0 )
  {
    std::pair< uint32_t , uint16_t > lPair ( ExtractTargetID ( aUri ) );
    mDeviceIPaddress = htonl ( lPair.first );
    mDevicePort = htons ( lPair.second );
    //log ( Debug() , ThisLocation() );
  }


  template < typename InnerProtocol >
  ControlHub< InnerProtocol >::~ControlHub()
  {
    //log ( Debug() , ThisLocation() );
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::preamble ( boost::shared_ptr< Buffers > aBuffers )
  {
    //log ( Debug() , ThisLocation() );
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
      boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
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
    return InnerProtocol::getPreambleSize() +2;
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::predispatch ( boost::shared_ptr< Buffers > aBuffers )
  {
    InnerProtocol::predispatch ( aBuffers );
    //log ( Debug() , ThisLocation() );
    boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
    tpreamble& lPreambles = mPreambles.back();
    uint32_t lByteCount ( aBuffers->sendCounter() );
    //     * ( lPreambles.mSendByteCountPtr ) = htonl ( lByteCount-4 );
    * ( lPreambles.mSendWordCountPtr ) = htons ( ( lByteCount-8 ) >>2 );
  }


  template < typename InnerProtocol >
  exception::exception* ControlHub< InnerProtocol >::validate ( uint8_t* aSendBufferStart ,
      uint8_t* aSendBufferEnd ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt )
  {
    //     aReplyStartIt++;
    aReplyStartIt++;
    uint32_t lReplyIPaddress ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) );

    if ( lReplyIPaddress != mDeviceIPaddress )
    {
      uhal::exception::ControlHubReturnedWrongAddress* lExc = new uhal::exception::ControlHubReturnedWrongAddress();
      log ( *lExc , "Returned IP address " , Integer ( lReplyIPaddress , IntFmt< hex , fixed >() ) ,
            " does not match that sent " , Integer ( mDeviceIPaddress, IntFmt< hex , fixed >() ) ,
            " for device with URI: " , this->uri() );
      boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
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
      boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();
      return lExc;
    }

    aReplyStartIt++;
    uint16_t lErrorCode ( ntohs ( * ( ( uint16_t* ) ( aReplyStartIt->first ) ) ) );

    if ( lErrorCode != 0 )
    {
      boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();

      if ( lErrorCode == 1 || lErrorCode == 3 || lErrorCode == 4 )
      {
        uhal::exception::ControlHubTargetTimeout* lExc = new uhal::exception::ControlHubTargetTimeout();
        log ( *lExc , "The ControlHub did not receive any response from the target with URI: ", this->uri() );
        log ( *lExc , "ControlHub error code is: ", Integer ( lErrorCode ) );
        return lExc ;
      }
      else if ( lErrorCode == 2 )
      {
        uhal::exception::ControlHubInternalTimeout* lExc = new uhal::exception::ControlHubInternalTimeout();
        log ( *lExc, "Internal timeout within the ControlHub for target with URI: ", this->uri() );
        return lExc;
      }
      else if ( lErrorCode == 5 )
      {
        uhal::exception::ControlHubReportedMalformedStatus* lExc = new uhal::exception::ControlHubReportedMalformedStatus();
        log ( *lExc , "ControlHub received malformed status packet from target with URI: " , this->uri() );
        return lExc;
      }
      else
      {
        uhal::exception::ControlHubUnknownErrorCode* lExc = new uhal::exception::ControlHubUnknownErrorCode();
        log ( *lExc , "Control Hub returned an unknown error code " , Integer ( lErrorCode, IntFmt< hex , fixed >() ),
              " for target with URI " , this->uri() , ". Please report this at https://svnweb.cern.ch/trac/cactus/newticket" );
        return lExc;
      }

      return false;
    }

    //aReplyStartIt++;
    // log ( Info() , "Control Hub has validated the packet headers" );
    {
      boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.pop_front();
    }
    return InnerProtocol::validate ( ( aSendBufferStart+=8 ) , aSendBufferEnd , ( ++aReplyStartIt ) , aReplyEndIt );
  }





  template < typename InnerProtocol >
  exception::exception* ControlHub< InnerProtocol >::validate ( boost::shared_ptr< Buffers > aBuffers )
  {
    return ClientInterface::validate ( aBuffers );
  }


  template < typename InnerProtocol >
  void ControlHub< InnerProtocol >::dispatchExceptionHandler()
  {
    log ( Info ,  ThisLocation() );
    {
      boost::lock_guard<boost::mutex> lPreamblesLock ( mPreamblesMutex );
      mPreambles.clear();
    }
    InnerProtocol::dispatchExceptionHandler();
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

