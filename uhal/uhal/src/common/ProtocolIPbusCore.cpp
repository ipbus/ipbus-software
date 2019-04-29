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


#include <algorithm>                            // for min
#include <ostream>                              // for operator<<
#include <stddef.h>                             // for NULL

#include <boost/shared_ptr.hpp>                 // for shared_ptr
#include <boost/thread/lock_guard.hpp>          // for lock_guard
#include <boost/thread/mutex.hpp>               // for mutex

#include "uhal/log/LogLevels.hpp"               // for BaseLogLevel, Debug
#include "uhal/log/log.hpp"
#include "uhal/log/log_inserters.integer.hpp"   // for Integer, _Integer
#include "uhal/log/log_inserters.quote.hpp"     // for Quote, _Quote
#include "uhal/Buffers.hpp"


namespace uhal
{

  std::ostream& operator<< ( std::ostream& aStr , const uhal::eIPbusTransactionType& aIPbusTransactionType )
  {
    switch ( aIPbusTransactionType )
    {
      case uhal::B_O_T:
        aStr << "\"Byte Order Transaction\"";
        break;
      case uhal::R_A_I:
        aStr << "\"Reserved Address Information\"";
        break;
      case uhal::NI_READ:
        aStr << "\"Non-incrementing Read\"";
        break;
      case uhal::READ:
        aStr << "\"Incrementing Read\"";
        break;
      case uhal::NI_WRITE:
        aStr << "\"Non-incrementing Write\"";
        break;
      case uhal::WRITE:
        aStr << "\"Incrementing Write\"";
        break;
      case uhal::RMW_SUM:
        aStr << "\"Read-Modify-Write Sum\"";
        break;
      case uhal::RMW_BITS:
        aStr << "\"Read-Modify-Write Bits\"";
        break;
      case uhal::CONFIG_SPACE_READ:
        aStr << "\"Configuration space read\"";
        break;
    }

    return aStr;
  }



  IPbusCore::IPbusCore ( const std::string& aId, const URI& aUri , const boost::posix_time::time_duration& aTimeoutPeriod ) :
    ClientInterface ( aId , aUri , aTimeoutPeriod ),
    mTransactionCounter ( 0x00000000 )
  {}


  IPbusCore::~IPbusCore()
  {}



  ValWord< uint32_t > IPbusCore::readConfigurationSpace ( const uint32_t& aAddr )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementReadConfigurationSpace ( aAddr );
  }

  ValWord< uint32_t > IPbusCore::readConfigurationSpace ( const uint32_t& aAddr, const uint32_t& aMask )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementReadConfigurationSpace ( aAddr, aMask );
  }


  exception::exception* IPbusCore::validate ( uint8_t* aSendBufferStart ,
      uint8_t* aSendBufferEnd ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt )
  {
    const uint8_t* lSendBufferFirstByte = aSendBufferStart;
    uint32_t lNrSendBytesProcessed = 0;
    uint32_t lNrReplyBytesValidated = 0;
    eIPbusTransactionType lSendIPbusTransactionType , lReplyIPbusTransactionType;
    IPbusDataWidth lSendDataWidth, lReplyDataWidth;
    uint32_t lSendWordCount , lReplyWordCount;
    uint32_t lSendTransactionId , lReplyTransactionId;
    uint8_t lSendInfoCode , lReplyInfoCode;

    do
    {
      log ( Debug() , "Validating IPbus transaction; send/received headers ",
        Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ), IntFmt< hex , fixed >() ), "/",
        Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ), IntFmt< hex , fixed >() ),
        " (", Integer ( lNrSendBytesProcessed ) , "/", Integer ( lNrReplyBytesValidated ) , " send/reply bytes already validated)" );

      if ( ! implementExtractHeader ( * ( ( uint32_t* ) ( aSendBufferStart ) ) , lSendIPbusTransactionType , lSendDataWidth , lSendWordCount , lSendTransactionId , lSendInfoCode ) )
      {
        uhal::exception::IPbusCoreUnparsableTransactionHeader* lExc = new uhal::exception::IPbusCoreUnparsableTransactionHeader();
        log ( *lExc , "Unable to parse send header ", Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ), IntFmt< hex , fixed >() ) ,
              ", (base address ", Integer ( * ( ( uint32_t* ) ( aSendBufferStart+4 ) ), IntFmt< hex , fixed >() ), ")" , 
              ", for URI " , Quote ( this->uri() ) , 
              ", ", Integer ( lNrSendBytesProcessed+1 ) , " bytes into IPbus payload" );
        return lExc;
      }

      if ( ! implementExtractHeader ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) , lReplyIPbusTransactionType , lReplyDataWidth , lReplyWordCount , lReplyTransactionId , lReplyInfoCode ) )
      {
        uhal::exception::IPbusCoreUnparsableTransactionHeader* lExc = new uhal::exception::IPbusCoreUnparsableTransactionHeader();
        log ( *lExc , "Unable to parse reply header ", Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ), IntFmt< hex , fixed >() ),
                      " from URI " , Quote ( this->uri() ),
                      ", ", Integer ( lNrReplyBytesValidated+1 ) , " bytes into IPbus reply payload." );
        log ( *lExc , "Original sent header was ", Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ), IntFmt< hex , fixed >() ) ,
                      ", for base address ", Integer ( * ( ( uint32_t* ) ( aSendBufferStart+4 ) ), IntFmt< hex , fixed >() ), 
                      ", ", Integer ( lNrSendBytesProcessed+1 ) , " bytes into IPbus send payload" );
        return lExc;
     }

      const bool lTransactionIdMismatch = (lSendTransactionId != lReplyTransactionId);
      const bool lTransactionTypeMismatch = (lSendIPbusTransactionType != lReplyIPbusTransactionType);
      const bool lTransactionDataWidthMismatch = (lSendDataWidth != lReplyDataWidth);
      if ( lTransactionIdMismatch or lTransactionTypeMismatch or lTransactionDataWidthMismatch )
      {
        uhal::exception::IPbusTransactionFieldsIncorrect* lExc = new uhal::exception::IPbusTransactionFieldsIncorrect();

        std::string lFields;
        if (lTransactionIdMismatch and lTransactionTypeMismatch and lTransactionDataWidthMismatch)
          lFields = "ID, type and data width";
        if (lTransactionIdMismatch and lTransactionTypeMismatch)
          lFields = "ID and type";
        if (lTransactionIdMismatch and lTransactionDataWidthMismatch)
          lFields = "ID and data width";
        if (lTransactionTypeMismatch and lTransactionDataWidthMismatch)
          lFields = "type and data width";
        else if (lTransactionIdMismatch)
          lFields = "ID";
        else if (lTransactionTypeMismatch)
          lFields = "type";
        else
          lFields = "data width";

        log ( *lExc , "Incorrect transaction ", lFields, " returned from URI ", Quote ( this->uri() ), ". Sent header was " ,
                      Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ) , IntFmt< hex , fixed >() ),
                      " (", Integer ( lNrSendBytesProcessed+1 ), " bytes into IPbus send payload), for base address " ,
                      Integer ( * ( ( uint32_t* ) ( aSendBufferStart+4 ) ), IntFmt< hex , fixed >() ),
                      ", but return header is " , Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ) , IntFmt< hex , fixed >() ) ,
                      ", " , Integer ( lNrReplyBytesValidated+1 ) , " bytes into IPbus send payload");
        return lExc;
      }

      if ( lReplyInfoCode )
      {
        uhal::exception::IPbusCoreResponseCodeSet* lExc = new uhal::exception::IPbusCoreResponseCodeSet();
        log ( *lExc , "Transaction header returned from URI " , Quote ( this->uri() ) , ", " ,
              Integer ( * ( ( uint32_t* ) ( aReplyStartIt->first ) ), IntFmt< hex , fixed >() ),
              " (ID = " , Integer ( lReplyTransactionId, IntFmt< hex , fixed >() ) ,
              ", type = " , lReplyIPbusTransactionType ,
              ", word count = " , Integer ( lReplyWordCount ) ,
              ") has response field = " , Integer ( lReplyInfoCode, IntFmt< hex , fixed >() ) ,
              " = '", TranslatedFmt<uint8_t>(lReplyInfoCode, getInfoCodeTranslator()) , "' indicating an error (" ,
              Integer ( lNrReplyBytesValidated+1 ) , " bytes into IPbus reply payload)" );
        log ( *lExc , "Original sent header was ", Integer ( * ( ( uint32_t* ) ( aSendBufferStart ) ), IntFmt< hex , fixed >() ) ,
                      ", for base address ", Integer ( * ( ( uint32_t* ) ( aSendBufferStart+4 ) ), IntFmt< hex , fixed >() ), 
                      ", " , Integer ( lNrSendBytesProcessed+1 ) , " bytes into IPbus send payload" );
        return lExc;
      }

  
      switch ( lSendIPbusTransactionType )
      {
        case B_O_T:
        case R_A_I:
          aSendBufferStart += ( 1<<2 );
          break;
        case NI_READ:
        case READ:
        case CONFIG_SPACE_READ:
          aSendBufferStart += ( 2<<2 );
          break;
        case NI_WRITE:
        case WRITE:
          aSendBufferStart += ( ( 2+lSendWordCount ) <<2 );
          if ( lSendDataWidth == DATA64 )
            aSendBufferStart += lSendWordCount << 2;
          break;
        case RMW_SUM:
          aSendBufferStart += ( ( lSendDataWidth == DATA32 ) ? 3<<2 : 4<<2 );
          break;
        case RMW_BITS:
          aSendBufferStart += ( ( lSendDataWidth == DATA32 ) ? 4<<2 : 6<<2 );
          break;
      }

      lNrSendBytesProcessed = aSendBufferStart - lSendBufferFirstByte;

      switch ( lReplyIPbusTransactionType )
      {
        case B_O_T:
        case NI_WRITE:
        case WRITE:
          lNrReplyBytesValidated += aReplyStartIt->second;
          aReplyStartIt++;
          break;
        case R_A_I:
        case NI_READ:
        case READ:
        case CONFIG_SPACE_READ:
        case RMW_SUM:
        case RMW_BITS:
          lNrReplyBytesValidated += ( aReplyStartIt->second + ( aReplyStartIt+1 )->second );
          aReplyStartIt+=2;
          break;
      }
    }
    while ( ( aSendBufferEnd - aSendBufferStart != 0 ) && ( aReplyEndIt - aReplyStartIt != 0 ) );

    log ( Debug() , "Validation Complete!" );
    return NULL;
  }
  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------


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
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    lBuffers->send ( implementCalculateHeader ( B_O_T , DATA32 , 0 , mTransactionCounter++ , requestTransactionInfoCode() ) );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->add ( lReply.first );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValHeader IPbusCore::implementWrite32 ( const uint32_t& aAddr, const uint32_t& aSource )
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
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( WRITE , DATA32 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    lBuffers->send ( aSource );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->add ( lReply.first );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    return lReply.first;
  }

  ValHeader IPbusCore::implementWrite64 ( const uint32_t& aAddr, const uint64_t& aSource )
  {
    log ( Debug() , "Write (64-bit) " , Integer ( aSource , IntFmt<hex,fixed>() ) , " to address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // WORD(31:0)
    // WORD(63:32)
    uint32_t lSendByteCount ( 4 << 2 );
    // IPbus reply packet format is:
    // HEADER
    uint32_t lReplyByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( WRITE , DATA64 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    lBuffers->send <uint64_t> ( aSource );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->add ( lReply.first );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    return lReply.first;
  }


  ValHeader IPbusCore::implementWriteBlock32 ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
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
    uint8_t* lSourcePtr ( ( uint8_t* ) ( aSource.empty() ? NULL : & ( aSource.at ( 0 ) ) ) );
    uint32_t lAddr ( aAddr );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    boost::shared_ptr< Buffers > lBuffers;

    do
    {
      lBuffers = checkBufferSpace ( lSendHeaderByteCount+lPayloadByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lSendBytesAvailableForPayload ( std::min ( 4*getMaxTransactionWordCount(), lSendBytesAvailable - lSendHeaderByteCount ) & 0xFFFFFFFC );

      lBuffers->send ( implementCalculateHeader ( lType , DATA32 , lSendBytesAvailableForPayload>>2 , mTransactionCounter++ , requestTransactionInfoCode() ) );
      lBuffers->send ( lAddr );
      if ( aSource.size() > 0 )
      {
        lBuffers->send ( lSourcePtr , lSendBytesAvailableForPayload );
        lSourcePtr += lSendBytesAvailableForPayload;
        lPayloadByteCount -= lSendBytesAvailableForPayload;
      }

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lSendBytesAvailableForPayload>>2 );
      }

      lReply.second->IPbusHeaders.push_back ( 0 );
      lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    }
    while ( lPayloadByteCount > 0 );

    lBuffers->add ( lReply.first ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply.first;
  }

  ValHeader IPbusCore::implementWriteBlock64 ( const uint32_t& aAddr, const std::vector< uint64_t >& aSource, const defs::BlockReadWriteMode& aMode )
  {
    log ( Debug() , "Write (64-bit) block of size " , Integer ( aSource.size() ) , " to address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // WORD 1 (31:0)
    // WORD 1 (63:32)
    // WORD 2 (31:0)
    // WORD 2 (63:32)
    // ....
    uint32_t lSendHeaderByteCount ( 2 << 2 );
    // IPbus reply packet format is:
    // HEADER
    uint32_t lReplyByteCount ( 1 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? WRITE : NI_WRITE );
    int32_t lPayloadByteCount ( aSource.size() << 3 );
    uint8_t* lSourcePtr ( ( uint8_t* ) ( aSource.empty() ? NULL : & ( aSource.at ( 0 ) ) ) );
    uint32_t lAddr ( aAddr );
    std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
    boost::shared_ptr< Buffers > lBuffers;

    do
    {
      lBuffers = checkBufferSpace ( lSendHeaderByteCount+lPayloadByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lSendBytesAvailableForPayload ( std::min ( 8*getMaxTransactionWordCount(), lSendBytesAvailable - lSendHeaderByteCount ) & 0xFFFFFFF8 );

      lBuffers->send ( implementCalculateHeader ( lType , DATA64 , lSendBytesAvailableForPayload>>3 , mTransactionCounter++ , requestTransactionInfoCode() ) );
      lBuffers->send ( lAddr );
      if ( aSource.size() > 0 )
      {
        lBuffers->send ( lSourcePtr , lSendBytesAvailableForPayload );
        lSourcePtr += lSendBytesAvailableForPayload;
        lPayloadByteCount -= lSendBytesAvailableForPayload;
      }

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lSendBytesAvailableForPayload>>3 );
      }

      lReply.second->IPbusHeaders.push_back ( 0 );
      lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    }
    while ( lPayloadByteCount > 0 );

    lBuffers->add ( lReply.first ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > IPbusCore::implementRead32 ( const uint32_t& aAddr, const uint32_t& aMask )
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
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( READ , DATA32 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 , aMask ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive ( lReply.second->value );
    return lReply.first;
  }

  ValWord< uint64_t > IPbusCore::implementRead64 ( const uint32_t& aAddr, const uint64_t& aMask )
  {
    log ( Debug() , "Read (64-bit) one unsigned word from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    uint32_t lSendByteCount ( 2 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    uint32_t lReplyByteCount ( 3 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( READ , DATA64 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    std::pair < ValWord<uint64_t> , _ValWord_<uint64_t>* > lReply ( CreateValWord64 ( 0 , aMask ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive <uint64_t> ( lReply.second->value );
    return lReply.first;
  }


  ValVector< uint32_t > IPbusCore::implementReadBlock32 ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
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
    uint8_t* lReplyPtr = ( uint8_t* ) ( aSize == 0 ? NULL : & ( lReply.second->value.at(0) ) );
    eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
    int32_t lPayloadByteCount ( aSize << 2 );
    uint32_t lAddr ( aAddr );
    boost::shared_ptr< Buffers > lBuffers;

    do
    {
      lBuffers = checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lReplyBytesAvailableForPayload ( std::min ( 4*getMaxTransactionWordCount(), lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFFC );
      lBuffers->send ( implementCalculateHeader ( lType , DATA32 , lReplyBytesAvailableForPayload>>2 , mTransactionCounter++ , requestTransactionInfoCode()
                                                ) );
      lBuffers->send ( lAddr );
      lReply.second->IPbusHeaders.push_back ( 0 );
      lBuffers->receive ( lReply.second->IPbusHeaders.back() );
      lBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
      lReplyPtr += lReplyBytesAvailableForPayload;
      lPayloadByteCount -= lReplyBytesAvailableForPayload;

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lReplyBytesAvailableForPayload>>2 );
      }
    }
    while ( lPayloadByteCount > 0 );

    lBuffers->add ( lReply.first ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply.first;
  }

  ValVector< uint64_t > IPbusCore::implementReadBlock64 ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  {
    log ( Debug() , "Read (64-bit) unsigned block of size " , Integer ( aSize ) , " from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
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
    std::pair < ValVector<uint64_t> , _ValVector_<uint64_t>* > lReply ( CreateValVector64 ( aSize ) );
    uint8_t* lReplyPtr = ( uint8_t* ) ( aSize == 0 ? NULL : & ( lReply.second->value.at(0) ) );
    eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
    int32_t lPayloadByteCount ( aSize << 3 );
    uint32_t lAddr ( aAddr );
    boost::shared_ptr< Buffers > lBuffers;

    do
    {
      lBuffers = checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
      uint32_t lReplyBytesAvailableForPayload ( std::min ( 8*getMaxTransactionWordCount(), lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFF8 );
      lBuffers->send ( implementCalculateHeader ( lType , DATA64 , lReplyBytesAvailableForPayload>>3 , mTransactionCounter++ , requestTransactionInfoCode()
                                                ) );
      lBuffers->send ( lAddr );
      lReply.second->IPbusHeaders.push_back ( 0 );
      lBuffers->receive ( lReply.second->IPbusHeaders.back() );
      lBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
      lReplyPtr += lReplyBytesAvailableForPayload;
      lPayloadByteCount -= lReplyBytesAvailableForPayload;

      if ( aMode == defs::INCREMENTAL )
      {
        lAddr += ( lReplyBytesAvailableForPayload>>3 );
      }
    }
    while ( lPayloadByteCount > 0 );

    lBuffers->add ( lReply.first ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  ValWord< uint32_t > IPbusCore::implementReadConfigurationSpace ( const uint32_t& aAddr, const uint32_t& aMask )
  {
    log ( Debug() , "Read one unsigned word from configuration space address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
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
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    lBuffers->send ( implementCalculateHeader ( CONFIG_SPACE_READ , DATA32 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 , aMask ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive ( lReply.second->value );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > IPbusCore::implementRMWbits32 ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
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
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( RMW_BITS , DATA32 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    lBuffers->send ( aANDterm );
    lBuffers->send ( aORterm );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive ( lReply.second->value );
    return lReply.first;
  }

  ValWord< uint64_t > IPbusCore::implementRMWbits64 ( const uint32_t& aAddr , const uint64_t& aANDterm , const uint64_t& aORterm )
  {
    log ( Debug() , "Read/Modify/Write bits (64-bit, and=" , Integer ( aANDterm , IntFmt<hex,fixed>() ) , ", or=" , Integer ( aORterm , IntFmt<hex,fixed>() ) , ") from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );
    // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // AND TERM
    // OR TERM
    uint32_t lSendByteCount ( 6 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    uint32_t lReplyByteCount ( 3 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( RMW_BITS , DATA64 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    lBuffers->send <uint64_t> ( aANDterm );
    lBuffers->send <uint64_t> ( aORterm );
    std::pair < ValWord<uint64_t> , _ValWord_<uint64_t>* > lReply ( CreateValWord64 ( 0 ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive ( lReply.second->value );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > IPbusCore::implementRMWsum32 ( const uint32_t& aAddr , const int32_t& aAddend )
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
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( RMW_SUM , DATA32 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    lBuffers->send ( static_cast< uint32_t > ( aAddend ) );
    std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > lReply ( CreateValWord ( 0 ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive ( lReply.second->value );
    return lReply.first;
  }

  ValWord< uint64_t > IPbusCore::implementRMWsum64 ( const uint32_t& aAddr , const int64_t& aAddend )
  {
    log ( Debug() , "Read/Modify/Write sum (64-bit, addend=" , Integer ( aAddend , IntFmt<hex,fixed>() ) , ") from address " , Integer ( aAddr , IntFmt<hex,fixed>() ) );     // IPbus packet format is:
    // HEADER
    // BASE ADDRESS
    // ADDEND
    uint32_t lSendByteCount ( 4 << 2 );
    // IPbus reply packet format is:
    // HEADER
    // WORD
    uint32_t lReplyByteCount ( 3 << 2 );
    uint32_t lSendBytesAvailable;
    uint32_t  lReplyBytesAvailable;
    boost::shared_ptr< Buffers > lBuffers = checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
    assert (lSendBytesAvailable >= lSendByteCount);
    assert (lReplyBytesAvailable >= lReplyByteCount);
    lBuffers->send ( implementCalculateHeader ( RMW_SUM , DATA64 , 1 , mTransactionCounter++ , requestTransactionInfoCode()
                                              ) );
    lBuffers->send ( aAddr );
    lBuffers->send <uint64_t> ( static_cast< uint64_t > ( aAddend ) );
    std::pair < ValWord<uint64_t> , _ValWord_<uint64_t>* > lReply ( CreateValWord64 ( 0 ) );
    lBuffers->add ( lReply.first );
    lReply.second->IPbusHeaders.push_back ( 0 );
    lBuffers->receive ( lReply.second->IPbusHeaders.back() );
    lBuffers->receive ( lReply.second->value );
    return lReply.first;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  void IPbusCore::dispatchExceptionHandler()
  {
    mTransactionCounter = 0;
    ClientInterface::dispatchExceptionHandler();
  }

}


