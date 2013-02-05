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

#include "uhal/ProtocolInterfaces.hpp"
// #include "uhal/performance.hpp"

#include "boost/date_time/posix_time/posix_time.hpp"

#include <cstring>

namespace uhal
{

  Buffers::Buffers ( const uint32_t& aMaxSendSize ) :
    mSendCounter ( 0 ),
    mReplyCounter ( 0 ),
    mSendBuffer ( new uint8_t[ aMaxSendSize ] )
  {
    logging();
  }



  Buffers::~Buffers()
  {
    logging();

    if ( mSendBuffer )
    {
      delete[] mSendBuffer;
      mSendBuffer = NULL;
    }
  }


  const uint32_t& Buffers::sendCounter()
  {
    logging();
    return mSendCounter;
  }

  const uint32_t& Buffers::replyCounter()
  {
    logging();
    return mReplyCounter;
  }



  uint8_t* Buffers::send ( const uint8_t* aPtr , const uint32_t& aSize )
  {
    logging();
    uint8_t* lStartPtr ( mSendBuffer+mSendCounter );
    memcpy ( lStartPtr , aPtr , aSize );
    mSendCounter += aSize;
    return lStartPtr;
  }


  void Buffers::receive ( uint8_t* aPtr , const uint32_t& aSize )
  {
    logging();
    mReplyBuffer.push_back ( std::make_pair ( aPtr , aSize ) );
    mReplyCounter += aSize;
  }

  void Buffers::add ( const ValHeader& aValMem )
  {
    logging();
    mValHeaders.push_back ( aValMem );
  }

  void Buffers::add ( const ValWord< uint32_t >& aValMem )
  {
    logging();
    mUnsignedValWords.push_back ( aValMem );
  }

  void Buffers::add ( const ValWord< int32_t >& aValMem )
  {
    logging();
    mSignedValWords.push_back ( aValMem );
  }

  void Buffers::add ( const ValVector< uint32_t >& aValMem )
  {
    logging();
    mUnsignedValVectors.push_back ( aValMem );
  }

  void Buffers::add ( const ValVector< int32_t >& aValMem )
  {
    logging();
    mSignedValVectors.push_back ( aValMem );
  }

  uint8_t* Buffers::getSendBuffer()
  {
    logging();
    return mSendBuffer;
  }

  std::deque< std::pair< uint8_t* , uint32_t > >& Buffers::getReplyBuffer()
  {
    logging();
    return mReplyBuffer;
  }


  void Buffers::validate()
  {
    logging();

    for ( std::deque< ValHeader >::iterator lIt = mValHeaders.begin() ; lIt != mValHeaders.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    for ( std::deque< ValWord< uint32_t > >::iterator lIt = mUnsignedValWords.begin() ; lIt != mUnsignedValWords.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    for ( std::deque< ValWord< int32_t > >::iterator lIt = mSignedValWords.begin() ; lIt != mSignedValWords.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    for ( std::deque< ValVector< uint32_t > >::iterator lIt = mUnsignedValVectors.begin() ; lIt != mUnsignedValVectors.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    for ( std::deque< ValVector< int32_t > >::iterator lIt = mSignedValVectors.begin() ; lIt != mSignedValVectors.end() ; ++lIt )
    {
      lIt->valid ( true );
    }
  }




  void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol )
  {
    logging();
    aTransportProtocol.mPackingProtocol = &aPackingProtocol;
    aPackingProtocol.mTransportProtocol = &aTransportProtocol;
  }




  TransportProtocol::TransportProtocol ( const boost::posix_time::time_duration& aTimeoutPeriod ) :
    mTimeoutPeriod ( aTimeoutPeriod )
  {
    logging();
  }


  TransportProtocol::~TransportProtocol()
  {
    logging();
  }


  void TransportProtocol::setTimeoutPeriod ( const boost::posix_time::time_duration& aTimeoutPeriod )
  {
    logging();
    mTimeoutPeriod = aTimeoutPeriod;
    log ( Info(), "timeout = ", boost::posix_time::to_simple_string ( mTimeoutPeriod ) );
  }

  const boost::posix_time::time_duration& TransportProtocol::getTimeoutPeriod()
  {
    logging();
    return mTimeoutPeriod;
  }




  PackingProtocol::PackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) :
    mTransportProtocol ( NULL ),
    mCurrentBuffers ( NULL ),
    mMaxSendSize ( aMaxSendSize ),
    mMaxReplySize ( aMaxReplySize )
  {
    logging();
  }


  PackingProtocol::~PackingProtocol()
  {
    logging();
    DeleteBuffer();
  }


  void PackingProtocol::DeleteBuffer()
  {
    logging();

    if ( mCurrentBuffers )
    {
      delete mCurrentBuffers;
      mCurrentBuffers = NULL;
    }
  }

  void PackingProtocol::Preamble( )
  {
    logging();
    log ( Debug() , "Preamble" );
    this->ByteOrderTransaction();
  }

  void PackingProtocol::Predispatch( )
  {
    logging();
  }


  void PackingProtocol::Dispatch( )
  {
    logging();
    log ( Debug() , "Dispatch" );

    if ( mCurrentBuffers )
    {
      if ( mCurrentBuffers->sendCounter() )
      {
        this->Predispatch();
        mTransportProtocol->Dispatch ( mCurrentBuffers );
        mCurrentBuffers = NULL; //Not deleting the underlying buffer, just flagging that we need a new buffer next time
        mTransportProtocol->Flush();
      }
    }
  }


  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  // NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
  // IT MUST ONLY USE LOCAL VARIABLES
  //            --- OR ---
  // IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  bool PackingProtocol::Validate ( uint8_t* aSendBufferStart ,
                                   uint8_t* aSendBufferEnd ,
                                   std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
                                   std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt )
  {
    logging();
    log ( Debug() , "Underlying Validation" );
    eIPbusTransactionType lSendIPbusTransactionType , lReplyIPbusTransactionType;
    uint32_t lSendWordCount , lReplyWordCount;
    uint32_t lSendTransactionId , lReplyTransactionId;
    uint8_t lSendResponseGood , lReplyResponseGood;

    do
    {
      if ( ! this->extractIPbusHeader ( * ( ( uint32_t* ) ( aSendBufferStart ) ) , lSendIPbusTransactionType , lSendWordCount , lSendTransactionId , lSendResponseGood ) )
      {
        log ( Error() , "Unable to parse send header " , Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ), IntFmt< hex , fixed >() ) );
        return false;
      }

      if ( ! this->extractIPbusHeader ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) , lReplyIPbusTransactionType , lReplyWordCount , lReplyTransactionId , lReplyResponseGood ) )
      {
        log ( Error() , "Unable to parse reply header " , Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ), IntFmt< hex , fixed >() ) );
        return false;
      }

      if ( lReplyResponseGood )
      {
        log ( Error() , "Returned Header, " , Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ), IntFmt< hex , fixed >() ),
              " ( transaction id = " , Integer ( lReplyTransactionId, IntFmt< hex , fixed >() ) ,
              ", transaction type = " , Integer ( ( uint8_t ) ( ( lReplyIPbusTransactionType >> 3 ) ), IntFmt< hex , fixed >() ) ,
              ", word count = " , Integer ( lReplyWordCount ) ,
              " ) had response field = " , Integer ( lReplyResponseGood, IntFmt< hex , fixed >() ) , " indicating an error" );
        return false;
      }

      if ( lSendIPbusTransactionType != lReplyIPbusTransactionType )
      {
        log ( Error() , "Returned Transaction Type " , Integer ( ( uint8_t ) ( ( lReplyIPbusTransactionType >> 3 ) ), IntFmt< hex , fixed >() ) ,
              " does not match that sent " , Integer ( ( uint8_t ) ( ( lSendIPbusTransactionType >> 3 ) ), IntFmt< hex , fixed >() ) );
        log ( Error() , "Sent Header was " , Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ) , IntFmt< hex , fixed >() ) ,
              " whilst Return Header was " , Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) , IntFmt< hex , fixed >() ) );
        return false;
      }

      if ( lSendTransactionId != lReplyTransactionId )
      {
        log ( Error() , "Returned Transaction Id " , Integer ( lReplyTransactionId ) ,
              " does not match that sent " , Integer ( lSendTransactionId ) );
        log ( Error() , "Sent Header was " , Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ) , IntFmt< hex , fixed >() ) ,
              " whilst Return Header was " , Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) , IntFmt< hex , fixed >() ) );
        return false;
      }

      switch ( lSendIPbusTransactionType )
      {
        case B_O_T:
        case R_A_I:
          aSendBufferStart += ( 1<<2 );
          break;
        case NI_READ:
        case READ:
          aSendBufferStart += ( 2<<2 );
          break;
        case NI_WRITE:
        case WRITE:
          aSendBufferStart += ( ( 2+lSendWordCount ) <<2 );
          break;
        case RMW_SUM:
          aSendBufferStart += ( 3<<2 );
          break;
        case RMW_BITS:
          aSendBufferStart += ( 4<<2 );
          break;
      }

      switch ( lReplyIPbusTransactionType )
      {
        case B_O_T:
        case NI_WRITE:
        case WRITE:
          aReplyStartIt++;
          break;
        case R_A_I:
        case NI_READ:
        case READ:
        case RMW_SUM:
        case RMW_BITS:
          aReplyStartIt+=2;
          break;
      }
    }
    while ( ( aSendBufferEnd - aSendBufferStart != 0 ) && ( aReplyEndIt - aReplyStartIt != 0 ) );

    log ( Debug() , "Validation Complete!" );
    return true;
  }
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------




  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  // NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
  // IT MUST ONLY USE LOCAL VARIABLES
  //            --- OR ---
  // IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  bool PackingProtocol::Validate ( Buffers* aBuffers )
  {
    logging();
    bool lRet = this->Validate ( aBuffers->getSendBuffer() ,
                                 aBuffers->getSendBuffer() +aBuffers->sendCounter() ,
                                 aBuffers->getReplyBuffer().begin() ,
                                 aBuffers->getReplyBuffer().end() );

    if ( lRet )
    {
      aBuffers->validate();
      delete aBuffers; //We have now checked the returned data and marked as valid the underlying memory. We can, therefore, delete the local storage and from this point onward, the validated memory will only exist if the user kept their own copy
    }

    return lRet;
  }
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------


  ValHeader PackingProtocol::Padding()
  {
    logging();
    log ( Debug() , "Padding" );
    // We do not need to check for space here as I have deliberately made the maximum reply buffer size too small by 8 words so that we can add up to 8 words of padding if it is needed for a block read
    mCurrentBuffers->send ( this->calculateIPbusHeader ( B_O_T , 0 ) );
    ValHeader lReply;
    lReply.mMembers->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->add ( lReply );
    mCurrentBuffers->receive ( lReply.mMembers->IPbusHeaders.back() );
    return lReply;
  }


  ValHeader PackingProtocol::ByteOrderTransaction()
  {
    logging();
    log ( Debug() , "Byte Order Transaction" );
    // IPbus send packet format is:
    // HEADER
    uint32_t lSendByteCount ( 1 << 2 );
    // IPbus reply packet format is:
    // HEADER
    uint32_t lReplyByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( this->calculateIPbusHeader ( B_O_T , 0 ) );
    ValHeader lReply;
    lReply.mMembers->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->add ( lReply );
    mCurrentBuffers->receive ( lReply.mMembers->IPbusHeaders.back() );
    return lReply;
  }

  ValHeader PackingProtocol::write ( const uint32_t& aAddr, const uint32_t& aSource )
  {
    logging();
    log ( Debug() , "Write " , Integer ( aSource , IntFmt<hex,fixed>() ) , " to address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // WORD
    uint32_t lSendByteCount ( 3 << 2 );
    // IPbus reply packet format is:
    // HEADER
    uint32_t lReplyByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( this->calculateIPbusHeader ( WRITE , 1 ) );
    mCurrentBuffers->send ( aAddr );
    mCurrentBuffers->send ( aSource );
    ValHeader lReply;
    lReply.mMembers->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->add ( lReply );
    mCurrentBuffers->receive ( lReply.mMembers->IPbusHeaders.back() );
    return lReply;
  }

  ValHeader PackingProtocol::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
  {
    logging();
    log ( Debug() , "Write block of size " , Integer ( aSource.size() ) , " to address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // WORD
    // WORD
    // ....
    uint32_t lSendHeaderByteCount ( 2 << 2 );
    // IPbus reply packet format is:
    // HEADER
    uint32_t lReplyByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? WRITE : NI_WRITE );
    int32_t lPayloadByteCount ( aSource.size() << 2 );
    uint8_t* lSourcePtr ( ( uint8_t* ) ( & ( aSource.at ( 0 ) ) ) );
    uint32_t lAddr ( aAddr );
    ValHeader lReply;

    while ( lPayloadByteCount > 0 )
    {
      this->checkBufferSpace ( lSendHeaderByteCount+lPayloadByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lSendBytesAvailableForPayload ( ( lSendBytesAvailable - lSendHeaderByteCount ) & 0xFFFFFFFC );
      //log ( Info() , "lSendBytesAvailable = " , Integer(lSendBytesAvailable) );
      //log ( Info() , "lSendBytesAvailableForPayload (bytes) = " , Integer(lSendBytesAvailableForPayload) );
      //log ( Info() , "lSendBytesAvailableForPayload (words) = " , Integer(lSendBytesAvailableForPayload>>2) );
      //log ( Info() , "lPayloadByteCount = " , Integer(lPayloadByteCount) );
      mCurrentBuffers->send ( this->calculateIPbusHeader ( lType , lSendBytesAvailableForPayload>>2 ) );
      mCurrentBuffers->send ( lAddr );
      mCurrentBuffers->send ( lSourcePtr , lSendBytesAvailableForPayload );
      lSourcePtr += lSendBytesAvailableForPayload;
      lPayloadByteCount -= lSendBytesAvailableForPayload;

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lSendBytesAvailableForPayload>>2 );
      }

      lReply.mMembers->IPbusHeaders.push_back ( 0 );
      mCurrentBuffers->receive ( lReply.mMembers->IPbusHeaders.back() );
    }

    mCurrentBuffers->add ( lReply ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  ValWord< uint32_t > PackingProtocol::read ( const uint32_t& aAddr, const uint32_t& aMask )
  {
    logging();
    log ( Debug() , "Read one unsigned word from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    uint32_t lSendByteCount ( 2 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    uint32_t lReplyByteCount ( 2 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( this->calculateIPbusHeader ( READ , 1 ) );
    mCurrentBuffers->send ( aAddr );
    ValWord< uint32_t > lReply ( 0 , aMask );
    mCurrentBuffers->add ( lReply );
    _ValWord_< uint32_t >& lReplyMem = * ( lReply.mMembers );
    lReplyMem.IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
    // mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
    mCurrentBuffers->receive ( lReplyMem.value );
    return lReply;
  }

  ValVector< uint32_t > PackingProtocol::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  {
    logging();
    log ( Debug() , "Read unsigned block of size " , Integer ( aSize ) , " from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    uint32_t lSendByteCount ( 2 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    // WORD
    // ....
    uint32_t lReplyHeaderByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    ValVector< uint32_t > lReply ( aSize );
    _ValVector_< uint32_t >& lReplyMem = * ( lReply.mMembers );
    uint8_t* lReplyPtr = ( uint8_t* ) ( & ( lReplyMem.value[0] ) );
    eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
    int32_t lPayloadByteCount ( aSize << 2 );
    uint32_t lAddr ( aAddr );

    while ( lPayloadByteCount > 0 )
    {
      this->checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lReplyBytesAvailableForPayload ( ( lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFFC );
      mCurrentBuffers->send ( this->calculateIPbusHeader ( lType , lReplyBytesAvailableForPayload>>2 ) );
      mCurrentBuffers->send ( lAddr );
      lReplyMem.IPbusHeaders.push_back ( 0 );
      mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
      mCurrentBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
      lReplyPtr += lReplyBytesAvailableForPayload;
      lPayloadByteCount -= lReplyBytesAvailableForPayload;

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lReplyBytesAvailableForPayload>>2 );
      }
    }

    mCurrentBuffers->add ( lReply ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // ValWord< int32_t > PackingProtocol::readSigned ( const uint32_t& aAddr, const uint32_t& aMask )
  // {
  // try
  // {
  // log ( Debug() , "Read one signed word from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
  // // IPbus packet format is:
  // // HEADER
  // // BASE ADDRESS
  // uint32_t lSendByteCount ( 2 << 2 );
  // // IPbus reply packet format is:
  // // HEADER
  // // WORD
  // uint32_t lReplyByteCount ( 2 << 2 );
  // uint32_t lSendBytesAvailable;
  // uint32_t  lReplyBytesAvailable;
  // this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
  // mCurrentBuffers->send ( this->calculateIPbusHeader ( READ , 1 ) );
  // mCurrentBuffers->send ( aAddr );
  // ValWord< int32_t > lReply ( 0 , aMask );
  // mCurrentBuffers->add ( lReply );
  // _ValWord_< int32_t >& lReplyMem = * ( lReply.mMembers );
  // mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
  // mCurrentBuffers->receive ( lReplyMem.value );
  // return lReply;
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }

  // ValVector< int32_t > PackingProtocol::readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  // {
  // try
  // {
  // log ( Debug() , "Read signed block of size " , Integer ( aSize ) , " from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
  // // IPbus packet format is:
  // // HEADER
  // // BASE ADDRESS
  // uint32_t lSendByteCount ( 2 << 2 );
  // // IPbus reply packet format is:
  // // HEADER
  // // WORD
  // // WORD
  // // ....
  // uint32_t lReplyHeaderByteCount ( 1 << 2 );
  // uint32_t lSendBytesAvailable;
  // uint32_t  lReplyBytesAvailable;
  // ValVector< int32_t > lReply ( aSize );
  // _ValVector_< int32_t >& lReplyMem = * ( lReply.mMembers );
  // uint8_t* lReplyPtr = ( uint8_t* ) ( & ( lReplyMem.value[0] ) );
  // eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
  // int32_t lPayloadByteCount ( aSize << 2 );
  // uint32_t lAddr ( aAddr );

  // while ( lPayloadByteCount > 0 )
  // {
  // this->checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
  // uint32_t lReplyBytesAvailableForPayload ( ( lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFFC );
  // mCurrentBuffers->send ( this->calculateIPbusHeader ( lType , lReplyBytesAvailableForPayload>>2 ) );
  // mCurrentBuffers->send ( lAddr );
  // lReplyMem.IPbusHeaders.push_back ( 0 );
  // mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
  // mCurrentBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
  // lReplyPtr += lReplyBytesAvailableForPayload;
  // lPayloadByteCount -= lReplyBytesAvailableForPayload;

  // if ( aMode == defs::INCREMENTAL )
  // {
  // lAddr += ( lReplyBytesAvailableForPayload>>2 );
  // }
  // }

  // mCurrentBuffers->add ( lReply ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
  // return lReply;
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }
  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // ValVector< uint32_t > PackingProtocol::readReservedAddressInfo ()
  // {
  // try
  // {
  // // IPbus packet format is:
  // // HEADER
  // uint32_t lSendByteCount ( 1 << 2 );
  // // IPbus reply packet format is:
  // // HEADER
  // // WORD
  // // WORD
  // uint32_t lReplyByteCount ( 3 << 2 );
  // uint32_t lSendBytesAvailable;
  // uint32_t  lReplyBytesAvailable;
  // this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
  // mCurrentBuffers->send ( this->calculateIPbusHeader ( R_A_I , 0 ) );
  // ValVector< uint32_t > lReply ( 2 );
  // mCurrentBuffers->add ( lReply );
  // _ValVector_< uint32_t >& lReplyMem = * ( lReply.mMembers );
  // lReplyMem.IPbusHeaders.push_back ( 0 );
  // mCurrentBuffers->receive ( lReplyMem.IPbusHeaders[0] );
  // mCurrentBuffers->receive ( ( uint8_t* ) ( & ( lReplyMem.value[0] ) ) , 2<<2 );
  // return lReply;
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );	// StdException ( aExc ).throwFrom( ThisLocation() );
  // }
  // }
  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > PackingProtocol::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
  {
    logging();
    log ( Debug() , "Read/Modify/Write bits (and=" , Integer ( aANDterm , IntFmt<hex,fixed>() ) , ", or=" , Integer ( aORterm , IntFmt<hex,fixed>() ) , ") from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // AND TERM
    // OR TERM
    uint32_t lSendByteCount ( 4 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    uint32_t lReplyByteCount ( 2 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( this->calculateIPbusHeader ( RMW_BITS , 1 ) );
    mCurrentBuffers->send ( aAddr );
    mCurrentBuffers->send ( aANDterm );
    mCurrentBuffers->send ( aORterm );
    ValWord< uint32_t > lReply ( 0 );
    mCurrentBuffers->add ( lReply );
    _ValWord_< uint32_t >& lReplyMem = * ( lReply.mMembers );
    lReplyMem.IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
    // mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
    mCurrentBuffers->receive ( lReplyMem.value );
    return lReply;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > PackingProtocol::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
  {
    logging();
    log ( Debug() , "Read/Modify/Write sum (addend=" , Integer ( aAddend , IntFmt<hex,fixed>() ) , ") from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );			// IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // ADDEND
    uint32_t lSendByteCount ( 3 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    uint32_t lReplyByteCount ( 2 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( this->calculateIPbusHeader ( RMW_SUM , 1 ) );
    mCurrentBuffers->send ( aAddr );
    mCurrentBuffers->send ( static_cast< uint32_t > ( aAddend ) );
    ValWord< uint32_t > lReply ( 0 );
    mCurrentBuffers->add ( lReply );
    _ValWord_< uint32_t >& lReplyMem = * ( lReply.mMembers );
    lReplyMem.IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
    // mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
    mCurrentBuffers->receive ( lReplyMem.value );
    return lReply;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  void PackingProtocol::checkBufferSpace ( const uint32_t& aRequestedSendSize , const uint32_t& aRequestedReplySize , uint32_t& aAvailableSendSize , uint32_t& aAvailableReplySize )
  {
    logging();
    log ( Debug() , "Checking buffer space" );

    if ( !mCurrentBuffers )
    {
      mCurrentBuffers = new Buffers ( mMaxSendSize );
      this->Preamble();
    }

    uint32_t lSendBufferFreeSpace ( mMaxSendSize - mCurrentBuffers->sendCounter() );
    uint32_t lReplyBufferFreeSpace ( mMaxReplySize - mCurrentBuffers->replyCounter() );
    // log ( Debug() , "Current buffer:\n" ,
    // " aRequestedSendSize " , Integer( aRequestedSendSize ) ,
    // " | aRequestedReplySize " , Integer( aRequestedReplySize ) ,
    // "\n" ,
    // " mMaxSendSize " , Integer( mMaxSendSize ) ,
    // " | mMaxReplySize " , Integer( mMaxReplySize ) ,
    // "\n" ,
    // " mCurrentBuffers->sendCounter() " , Integer( mCurrentBuffers->sendCounter() ) ,
    // " | mCurrentBuffers->replyCounter() " , Integer( mCurrentBuffers->replyCounter() ) ,
    // "\n" ,
    // " lSendBufferFreeSpace " , Integer(lSendBufferFreeSpace) ,
    // " | lReplyBufferFreeSpace " , Integer(lReplyBufferFreeSpace)
    // );

    if ( ( aRequestedSendSize <= lSendBufferFreeSpace ) && ( aRequestedReplySize <= lReplyBufferFreeSpace ) )
    {
      aAvailableSendSize = aRequestedSendSize;
      aAvailableReplySize = aRequestedReplySize;
      return;
    }

    if ( ( lSendBufferFreeSpace > 16 ) && ( lReplyBufferFreeSpace > 16 ) )
    {
      aAvailableSendSize = lSendBufferFreeSpace;
      aAvailableReplySize = lReplyBufferFreeSpace;
      return;
    }

    log ( Debug() , "Triggering automated dispatch" );
    this->Predispatch();
    mTransportProtocol->Dispatch ( mCurrentBuffers );
    mCurrentBuffers = new Buffers ( mMaxSendSize );
    this->Preamble();
    lSendBufferFreeSpace = mMaxSendSize - mCurrentBuffers->sendCounter();
    lReplyBufferFreeSpace = mMaxReplySize - mCurrentBuffers->replyCounter();
    // log ( Debug() , "Newly created buffer:\n" ,
    // " aRequestedSendSize " , Integer( aRequestedSendSize ) ,
    // " | aRequestedReplySize " , Integer( aRequestedReplySize ) ,
    // "\n" ,
    // " mMaxSendSize " , Integer( mMaxSendSize ) ,
    // " | mMaxReplySize " , Integer( mMaxReplySize ) ,
    // "\n" ,
    // " mCurrentBuffers->sendCounter() " , Integer( mCurrentBuffers->sendCounter() ) ,
    // " | mCurrentBuffers->replyCounter() " , Integer( mCurrentBuffers->replyCounter() ) ,
    // "\n" ,
    // " lSendBufferFreeSpace " , Integer(lSendBufferFreeSpace) ,
    // " | lReplyBufferFreeSpace " , Integer(lReplyBufferFreeSpace)
    // );

    if ( ( aRequestedSendSize <= lSendBufferFreeSpace ) && ( aRequestedReplySize <= lReplyBufferFreeSpace ) )
    {
      aAvailableSendSize = aRequestedSendSize;
      aAvailableReplySize = aRequestedReplySize;
      return;
    }

    aAvailableSendSize = lSendBufferFreeSpace;
    aAvailableReplySize = lReplyBufferFreeSpace;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}


