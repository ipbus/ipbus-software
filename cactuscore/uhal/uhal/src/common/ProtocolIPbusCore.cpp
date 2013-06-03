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

#include "uhal/ProtocolIPbusCore.hpp"

#include "boost/date_time/posix_time/posix_time.hpp"

#include <cstring>

namespace uhal
{


  IPbusCore::IPbusCore ( const std::string& aId, const URI& aUri , const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize , const boost::posix_time::time_duration& aTimeoutPeriod ) :
    ClientInterface ( aId , aUri ),
    mTransactionCounter ( 0x00000000 ),
    mMaxSendSize ( aMaxSendSize<<2 ),
    mMaxReplySize ( aMaxReplySize<<2 ),
    mTimeoutPeriod ( aTimeoutPeriod )
  {}


  IPbusCore::~IPbusCore()
  {}


  // void IPbusCore::preamble( )
  // {
  //
  // log ( Debug() , "preamble" );
  // ByteOrderTransaction();
  // }




  // void IPbusCore::Dispatch( )
  // {
  //
  // log ( Debug() , "Dispatch" );

  // if ( mCurrentBuffers )
  // {
  // if ( mCurrentBuffers->sendCounter() )
  // {
  // predispatch();
  // mTransportProtocol->Dispatch ( mCurrentBuffers );
  // mCurrentBuffers = NULL; //Not deleting the underlying buffer, just flagging that we need a new buffer next time
  // mTransportProtocol->Flush();
  // }
  // }
  // }

  bool IPbusCore::validate ( uint8_t* aSendBufferStart ,
                             uint8_t* aSendBufferEnd ,
                             std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
                             std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt )
  {
    log ( Debug() , "IPbusCore Validation" );
    eIPbusTransactionType lSendIPbusTransactionType , lReplyIPbusTransactionType;
    uint32_t lSendWordCount , lReplyWordCount;
    uint32_t lSendTransactionId , lReplyTransactionId;
    uint8_t lSendResponseGood , lReplyResponseGood;

    // log ( Debug() , ThisLocation() );
    do
    {
      if ( ! implementExtractHeader ( * ( ( uint32_t* ) ( aSendBufferStart ) ) , lSendIPbusTransactionType , lSendWordCount , lSendTransactionId , lSendResponseGood ) )
      {
        log ( Error() , "Unable to parse send header " , Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ), IntFmt< hex , fixed >() ) );
        return false;
      }

      if ( ! implementExtractHeader ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) , lReplyIPbusTransactionType , lReplyWordCount , lReplyTransactionId , lReplyResponseGood ) )
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

    // log ( Info() , "IPbus has validated the packet paylod" );
    log ( Debug() , "Validation Complete!" );
    return true;
  }
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------




  // // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  // // NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
  // // IT MUST ONLY USE LOCAL VARIABLES
  // //            --- OR ---
  // // IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
  // // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  // bool IPbusCore::Validate ( Buffers* aBuffers )
  // {
  //
  // bool lRet = Validate ( aBuffers->getSendBuffer() ,
  // aBuffers->getSendBuffer() +aBuffers->sendCounter() ,
  // aBuffers->getReplyBuffer().begin() ,
  // aBuffers->getReplyBuffer().end() );

  // if ( lRet )
  // {
  // aBuffers->validate();
  // delete aBuffers; //We have now checked the returned data and marked as valid the underlying memory. We can, therefore, delete the local storage and from this point onward, the validated memory will only exist if the user kept their own copy
  // }

  // return lRet;
  // }

  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValHeader IPbusCore::implementBOT()
  {
    log ( Debug() , "Byte Order Transaction" );
    // IPbus send packet format is:
    // HEADER
    uint32_t lSendByteCount ( 1 << 2 );
    // IPbus reply packet format is:
    // HEADER
    uint32_t lReplyByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( implementCalculateHeader ( B_O_T , 0 , mTransactionCounter++ , requestTransactionInfoCode() ) );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    lReply.second->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->add ( lReply.first );
    mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValHeader IPbusCore::implementWrite ( const uint32_t& aAddr, const uint32_t& aSource )
  {
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
    checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( implementCalculateHeader ( WRITE , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                                     ) );
    mCurrentBuffers->send ( aAddr );
    mCurrentBuffers->send ( aSource );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    lReply.second->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->add ( lReply.first );
    mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
    return lReply.first;
  }

  ValHeader IPbusCore::implementWriteBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
  {
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
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );

    while ( lPayloadByteCount > 0 )
    {
      checkBufferSpace ( lSendHeaderByteCount+lPayloadByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lSendBytesAvailableForPayload ( std::min ( 4*getMaxTransactionWordCount(), lSendBytesAvailable - lSendHeaderByteCount ) & 0xFFFFFFFC );
      log ( Info() , std::string ( 100,'-' ) );
      log ( Info() , ThisLocation(), ", Buffer: " , Pointer ( & ( *mCurrentBuffers ) ) );
      log ( Info() , "lSendBytesAvailable: " , Integer ( lSendBytesAvailable )  ,
            " | lSendBytesAvailableForPayload (bytes): " , Integer ( lSendBytesAvailableForPayload )  ,
            " | lSendBytesAvailableForPayload (words): " , Integer ( lSendBytesAvailableForPayload>>2 ) ,
            " | lPayloadByteCount = " , Integer ( lPayloadByteCount ) );
      mCurrentBuffers->send ( implementCalculateHeader ( lType , lSendBytesAvailableForPayload>>2 , mTransactionCounter++ , requestTransactionInfoCode() ) );
      mCurrentBuffers->send ( lAddr );
      mCurrentBuffers->send ( lSourcePtr , lSendBytesAvailableForPayload );
      lSourcePtr += lSendBytesAvailableForPayload;
      lPayloadByteCount -= lSendBytesAvailableForPayload;

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lSendBytesAvailableForPayload>>2 );
      }

      lReply.second->IPbusHeaders.push_back ( 0 );
      mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
    }

    mCurrentBuffers->add ( lReply.first ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  ValWord< uint32_t > IPbusCore::implementRead ( const uint32_t& aAddr, const uint32_t& aMask )
  {
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
    checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( implementCalculateHeader ( READ , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                                     ) );
    mCurrentBuffers->send ( aAddr );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 , aMask ) );
    mCurrentBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
    mCurrentBuffers->receive ( lReply.second->value );
    return lReply.first;
  }

  ValVector< uint32_t > IPbusCore::implementReadBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  {
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
    std::pair < ValVector<uint32_t> , _ValVector_<uint32_t>* > lReply ( CreateValVector ( aSize ) );
    uint8_t* lReplyPtr = ( uint8_t* ) ( & ( lReply.second->value[0] ) );
    eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
    int32_t lPayloadByteCount ( aSize << 2 );
    uint32_t lAddr ( aAddr );

    while ( lPayloadByteCount > 0 )
    {
      checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lReplyBytesAvailableForPayload ( std::min ( 4*getMaxTransactionWordCount(), lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFFC );
      mCurrentBuffers->send ( implementCalculateHeader ( lType , lReplyBytesAvailableForPayload>>2 , mTransactionCounter++ , requestTransactionInfoCode()
                                                       ) );
      mCurrentBuffers->send ( lAddr );
      lReply.second->IPbusHeaders.push_back ( 0 );
      mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
      mCurrentBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
      lReplyPtr += lReplyBytesAvailableForPayload;
      lPayloadByteCount -= lReplyBytesAvailableForPayload;

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lReplyBytesAvailableForPayload>>2 );
      }
    }

    mCurrentBuffers->add ( lReply.first ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > IPbusCore::implementRMWbits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
  {
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
    checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( implementCalculateHeader ( RMW_BITS , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                                     ) );
    mCurrentBuffers->send ( aAddr );
    mCurrentBuffers->send ( aANDterm );
    mCurrentBuffers->send ( aORterm );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 ) );
    mCurrentBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
    mCurrentBuffers->receive ( lReply.second->value );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > IPbusCore::implementRMWsum ( const uint32_t& aAddr , const int32_t& aAddend )
  {
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
    checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    mCurrentBuffers->send ( implementCalculateHeader ( RMW_SUM , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                                     ) );
    mCurrentBuffers->send ( aAddr );
    mCurrentBuffers->send ( static_cast< uint32_t > ( aAddend ) );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 ) );
    mCurrentBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    mCurrentBuffers->receive ( lReply.second->IPbusHeaders.back() );
    mCurrentBuffers->receive ( lReply.second->value );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  void IPbusCore::checkBufferSpace ( const uint32_t& aRequestedSendSize , const uint32_t& aRequestedReplySize , uint32_t& aAvailableSendSize , uint32_t& aAvailableReplySize )
  {
    log ( Debug() , "Checking buffer space" );
    //if there are no existing buffers in the pool, create them
    CreateFillingBuffer ( );
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
    this->unflushedDispatch();
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


  void IPbusCore::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
  {
    if ( aTimeoutPeriod == 0 )
    {
      mTimeoutPeriod = boost::posix_time::pos_infin;
    }
    else
    {
      mTimeoutPeriod = boost::posix_time::milliseconds ( aTimeoutPeriod );
    }
  }

  uint64_t IPbusCore::getTimeoutPeriod()
  {
    return mTimeoutPeriod.total_milliseconds();
  }


  uint32_t IPbusCore::getMaxSendSize()
  {
    return mMaxSendSize;
  }

  uint32_t IPbusCore::getMaxReplySize()
  {
    return mMaxReplySize;
  }


  void IPbusCore::dispatchExceptionHandler()
  {
    log ( Info() , ThisLocation() );
    mTransactionCounter = 0;
    ClientInterface::dispatchExceptionHandler();
  }

}


