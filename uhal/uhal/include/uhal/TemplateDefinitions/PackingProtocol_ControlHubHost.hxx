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

// #include <uhal/performance.hpp>

namespace uhal
{

  template< eIPbusProtocolVersion IPbusProtocolVersion >

  ControlHubHostPackingProtocol<  IPbusProtocolVersion >::ControlHubHostPackingProtocol ( const uint32_t& aDeviceIPaddr , const uint16_t& aDevicePort , const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) :
    PackingProtocol ( aMaxSendSize<<2 , aMaxReplySize<<2 ),
    mDeviceIPaddress ( htonl ( aDeviceIPaddr ) ),
    mDevicePort ( htons ( aDevicePort ) ),
    mTransactionCounter ( 0 )
  {
    logging();
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  ControlHubHostPackingProtocol<  IPbusProtocolVersion >::~ControlHubHostPackingProtocol()
  {
    logging();
  }

  template< eIPbusProtocolVersion IPbusProtocolVersion >
  uint32_t ControlHubHostPackingProtocol<  IPbusProtocolVersion >::calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount )
  {
    logging();
    return IPbusHeaderHelper<IPbusProtocolVersion>::calculate ( aType , aWordCount , mTransactionCounter++ );
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  bool ControlHubHostPackingProtocol<  IPbusProtocolVersion >::extractIPbusHeader (
    const uint32_t& aHeader ,
    eIPbusTransactionType& aType ,
    uint32_t& aWordCount ,
    uint32_t& aTransactionId ,
    uint8_t& aResponseGood )
  {
    logging();
    return IPbusHeaderHelper<IPbusProtocolVersion>::extract ( aHeader , aType , aWordCount , aTransactionId , aResponseGood );
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  void ControlHubHostPackingProtocol<  IPbusProtocolVersion >::Preamble( )
  {
    logging();
    // 12 bytes form the preamble:
    // Byte-count (4 bytes) will be updated before transmission in Predispatch
    // Device IP address (4 bytes)
    // Device Port number (2 bytes)
    // Word-count (2 bytes) will be updated before transmission in Predispatch
    // 16 bytes form the preamble reply:
    // Total Byte-count (4 bytes)
    // Chunk Byte-count (4 bytes)
    // Device IP address (4 bytes)
    // Device Port number (2 bytes)
    // Error code (2 bytes)
    tPreamble* lPreamble;
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );
      mPreambles.push_back ( tPreamble() );
      lPreamble = & mPreambles.back();
    }
    lPreamble->mSendByteCountPtr = ( uint32_t* ) ( mCurrentBuffers->send ( ( uint32_t ) ( 0 ) ) );
    mCurrentBuffers->send ( mDeviceIPaddress );
    mCurrentBuffers->send ( mDevicePort );
    lPreamble->mSendWordCountPtr = ( uint16_t* ) ( mCurrentBuffers->send ( ( uint16_t ) ( 0 ) ) );
    mCurrentBuffers->receive ( lPreamble->mReplyTotalByteCounter );
    mCurrentBuffers->receive ( lPreamble->mReplyChunkByteCounter );
    mCurrentBuffers->receive ( lPreamble->mReplyDeviceIPaddress );
    mCurrentBuffers->receive ( lPreamble->mReplyDevicePort );
    mCurrentBuffers->receive ( lPreamble->mReplyErrorCode );
    PackingProtocol::Preamble();
  }

  template< eIPbusProtocolVersion IPbusProtocolVersion >
  void ControlHubHostPackingProtocol<  IPbusProtocolVersion >::Predispatch( )
  {
    logging();
    tPreamble* lPreamble;
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );
      lPreamble = & mPreambles.back();
    }
    uint32_t lWords ( mCurrentBuffers->sendCounter()  >> 2 );

    if ( lWords < 11 ) // 8 words of data + 3 words of preamble
    {
      log ( Info() , "Adding " , Integer ( 11 - lWords ) , " words of padding." );

      for ( ; lWords != 11 ; ++lWords )
      {
        this->Padding();
      }
    }

    uint32_t lByteCount ( mCurrentBuffers->sendCounter() );
    *lPreamble->mSendByteCountPtr = htonl ( lByteCount-4 );
    *lPreamble->mSendWordCountPtr = htons ( ( lByteCount-12 ) >>2 );
  }


  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  // NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
  // IT MUST ONLY USE LOCAL VARIABLES
  //            --- OR ---
  // IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  template< eIPbusProtocolVersion IPbusProtocolVersion >
  bool ControlHubHostPackingProtocol< IPbusProtocolVersion >::Validate ( Buffers* aBuffers )
  {
    logging();
    uint8_t* lSendBuffer ( aBuffers->getSendBuffer() );
    uint8_t* lSendBufferEnd ( lSendBuffer+aBuffers->sendCounter() );
    lSendBuffer += 12;
    std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyIt ( aBuffers->getReplyBuffer().begin() );
    std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyEnd ( aBuffers->getReplyBuffer().end() );
    tPreamble* lPreamble;
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );
      lPreamble = & mPreambles.front();
    }
    /*		log ( Info() , "Byte Count 1 : " , Integer ( *(( uint32_t* )( lReplyIt->first )) ) ,
    								" : Memory : " , Integer ( lPreamble->mReplyTotalByteCounter ) ,
    								" : Reply counter : " , Integer ( aBuffers->replyCounter() )
    		);*/
    lReplyIt++;
    /*		log ( Info() , "Byte Count 2 : " , Integer ( *(( uint32_t* )( lReplyIt->first )) )  ,
    								" : Memory : " , Integer ( lPreamble->mReplyChunkByteCounter ) );*/
    lReplyIt++;

    /*		log ( Info() , "IP : " , Integer ( *(( uint32_t* )( lReplyIt->first )) )  ,
    								" : Memory : " , Integer ( lPreamble->mReplyDeviceIPaddress ) );*/
    if ( lPreamble->mReplyDeviceIPaddress != mDeviceIPaddress )
    {
      log ( Error() , "Returned IP address " , Integer ( lPreamble->mReplyDeviceIPaddress , IntFmt< hex , fixed >() ) ,
            " does not match that sent " , Integer ( mDeviceIPaddress, IntFmt< hex , fixed >() ) );
      return false;
    }

    lReplyIt++;

    /*		log ( Info() , "PORT : " , Integer  ( *(( uint16_t* )( lReplyIt->first )), IntFmt< hex , fixed >() )  ,
    								" : Memory : " , Integer  ( lPreamble->mReplyDevicePort, IntFmt< hex , fixed >() ) );*/
    if ( lPreamble->mReplyDevicePort != mDevicePort )
    {
      log ( Error() , "Returned Port number " , Integer ( lPreamble->mReplyDevicePort ) ,
            " does not match that sent " , Integer ( mDevicePort ) );
      return false;
    }

    lReplyIt++;

    /*		log ( Info() , "Error code : " , Integer ( *(( uint16_t* )( lReplyIt->first )) )  ,
    								" : Memory : " , Integer ( lPreamble->mReplyErrorCode ) );*/
    if ( lPreamble->mReplyErrorCode != 0 )
    {
      log ( Error() , "Control Hub reported error code " , Integer ( lPreamble->mReplyErrorCode, IntFmt< hex , fixed >() ) );
      return false;
    }

    lReplyIt++;
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );
      mPreambles.pop_front();
    }
    bool lRet =  PackingProtocol::Validate ( lSendBuffer ,
                 lSendBufferEnd ,
                 lReplyIt ,
                 lReplyEnd );

    if ( lRet )
    {
      aBuffers->validate();
      delete aBuffers; //We have now checked the returned data and marked as valid the underlying memory. We can, therefore, delete the local storage and from this point onward, the validated memory will only exist if the user kept their own copy
    }

    return lRet;
  }

}
