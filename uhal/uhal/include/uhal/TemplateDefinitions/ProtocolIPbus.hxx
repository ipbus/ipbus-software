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

#include "boost/date_time/posix_time/posix_time.hpp"

#include <cstring>

namespace uhal
{

  // --------------------------------------------------------------------------------------------------------------------------------------------------------------
  template< uint8_t IPbus_minor , uint32_t buffer_size >
  IPbus< 1 , IPbus_minor , buffer_size >::IPbus ( const std::string& aId, const URI& aUri ) :
    IPbusCore ( aId , aUri , buffer_size , buffer_size , boost::posix_time::seconds ( 1 ) ),
    mSendPadding ( 8 , implementCalculateHeader ( B_O_T , 0 , 0 ) ),
    mReplyPadding ( 8 , 0x00000000 )
  {}


  template< uint8_t IPbus_minor , uint32_t buffer_size >
  IPbus< 1 , IPbus_minor , buffer_size >::~IPbus()
  {}


  template< uint8_t IPbus_minor , uint32_t buffer_size >
  void IPbus< 1 , IPbus_minor , buffer_size >::preamble( )
  {
    implementBOT();
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  void IPbus< 1 , IPbus_minor , buffer_size >::predispatch( )
  {
    logging();
    uint32_t lWords ( mCurrentFillingBuffers->sendCounter()  >> 2 );

    if ( lWords < 8 )
    {
      uint32_t lPaddingWords ( 8 - lWords );
      log ( Debug() , "Adding " , Integer ( lPaddingWords ) , " words of padding." );
      mCurrentFillingBuffers->send ( ( uint8_t* ) ( & ( mSendPadding[0] ) ) , lPaddingWords<<2 );
      mCurrentFillingBuffers->receive ( ( uint8_t* ) ( & ( mReplyPadding[0] ) ) , lPaddingWords<<2 );
      /*
            for ( ; lWords != 8 ; ++lWords )
            {
      		// We do not need to check for space here as this condition is only met when the current filling buffer is severely underfull
              mCurrentFillingBuffers->send ( CalculateHeader ( B_O_T , 0 ) );
      		std::pair < ValHeader , _ValHeader_* > lReply ( CreateValHeader() );
      		lReply.second->IPbusHeaders.push_back ( 0 );
      		mCurrentFillingBuffers->add ( lReply.first );
      		mCurrentFillingBuffers->receive ( lReply.second->IPbusHeaders.back() );
            }
      */
    }
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  uint32_t IPbus< 1 , IPbus_minor , buffer_size >::CalculateHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId )
  {
    logging();
    uint8_t lType ( 0x00 );

    switch ( aType )
    {
      case B_O_T :
        lType = 0xF8;
        break;
      case READ :
        lType = 0x18;
        break;
      case WRITE :
        lType = 0x20;
        break;
      case RMW_BITS :
        lType = 0x28;
        break;
      case RMW_SUM :
        lType = 0x30;
        break;
      case R_A_I :
        lType = 0xF0;
        break;
      case NI_READ :
        lType = 0x40;
        break;
      case NI_WRITE :
        lType = 0x48;
        break;
    }

    return ( 0x10000000 | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | lType );
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  bool IPbus< 1 , IPbus_minor , buffer_size >::ExtractHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aInfoCode )
  {
    logging();

    try
    {
      switch ( aHeader & 0xF8 )
      {
        case 0xF8 :
          aType = B_O_T;
          break;
        case 0x18 :
          aType = READ;
          break;
        case 0x20 :
          aType = WRITE;
          break;
        case 0x28 :
          aType = RMW_BITS;
          break;
        case 0x30 :
          aType = RMW_SUM;
          break;
        case 0xF0 :
          aType = R_A_I;
          break;
        case 0x40 :
          aType = NI_READ;
          break;
        case 0x48 :
          aType = NI_WRITE;
          break;
        default:
          log ( Error() , "Unknown IPbus-header " , Integer ( uint8_t ( ( aHeader & 0xF8 ) ) , IntFmt<hex,fixed>() ) );
          throw exception::ValidationError();
      }

      aWordCount = ( aHeader >> 8 ) & 0x1ff;
      aTransactionId = ( aHeader >> 17 ) & 0x7ff;
      aInfoCode = aHeader & 0x3;
      return true;
    }
    catch ( const std::exception& aExc )
    {
      return false;
    }
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  uint32_t IPbus< 1 , IPbus_minor , buffer_size >::implementCalculateHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId )
  {
    return IPbus< 1 , IPbus_minor , buffer_size >::CalculateHeader ( aType , aWordCount , aTransactionId );
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  bool IPbus< 1 , IPbus_minor , buffer_size >::implementExtractHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aInfoCode )
  {
    return IPbus< 1 , IPbus_minor , buffer_size >::ExtractHeader ( aHeader , aType , aWordCount , aTransactionId , aInfoCode );
  }
  // --------------------------------------------------------------------------------------------------------------------------------------------------------------


  // --------------------------------------------------------------------------------------------------------------------------------------------------------------
  template< uint8_t IPbus_minor , uint32_t buffer_size >
  IPbus< 2 , IPbus_minor , buffer_size >::IPbus ( const std::string& aId, const URI& aUri ) :
    IPbusCore ( aId , aUri , buffer_size , buffer_size , boost::posix_time::seconds ( 1 ) ),
    mPacketCounter ( 1 )
  {}


  template< uint8_t IPbus_minor , uint32_t buffer_size >
  IPbus< 2 , IPbus_minor , buffer_size >::~IPbus()
  {}

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  void IPbus< 2 , IPbus_minor , buffer_size >:: preamble( )
  {
    mSendPacketHeader.push_back ( 0x200000F0 | ( ( mPacketCounter++&0xffff ) <<8 ) );
    mCurrentFillingBuffers->send ( mSendPacketHeader.back() );
    mReceivePacketHeader.push_back ( 0x00000000 );
    mCurrentFillingBuffers->receive ( mReceivePacketHeader.back() );
  }


  template< uint8_t IPbus_minor , uint32_t buffer_size >
  bool IPbus< 2 , IPbus_minor , buffer_size >::validate ( uint8_t* aSendBufferStart ,
      uint8_t* aSendBufferEnd ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
      std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt )
  {
    logging();

    if ( *aSendBufferStart != * ( uint32_t* ) ( aReplyStartIt ->first ) )
    {
      log ( Error() , "Returned PacketHeader " , Integer ( * ( uint32_t* ) ( aReplyStartIt ->first ) ) ,
            " does not match that sent " , Integer ( *aSendBufferStart ) );
      return false;
    }

    return IPbus::validate ( ( ++aSendBufferStart ) , aSendBufferEnd , ( ++aReplyStartIt ) , aReplyEndIt );
  }


  template< uint8_t IPbus_minor , uint32_t buffer_size >
  uint32_t IPbus< 2 , IPbus_minor , buffer_size >::CalculateHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId )
  {
    logging();
    uint8_t lType ( 0x00 );

    switch ( aType )
    {
      case B_O_T :
        log ( Error() , "Byte-Order-Transaction undefined in IPbus version 2" );
        throw exception::ValidationError();
      case READ :
        lType = 0x00;
        break;
      case WRITE :
        lType = 0x10;
        break;
      case NI_READ :
        lType = 0x20;
        break;
      case NI_WRITE :
        lType = 0x30;
        break;
      case RMW_BITS :
        lType = 0x40;
        break;
      case RMW_SUM :
        lType = 0x50;
        break;
      case R_A_I :
        log ( Error() , "Reserved-Address-Info undefined in IPbus version 2" );
        throw exception::ValidationError();
    }

    return ( 0x2000000F | ( ( aTransactionId&0xfff ) <<16 ) | ( ( aWordCount&0xff ) <<8 ) | lType );
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  bool IPbus< 2 , IPbus_minor , buffer_size >::ExtractHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aInfoCode )
  {
    logging();

    try
    {
      switch ( aHeader & 0xF0 )
      {
        case 0x00 :
          aType = READ;
          break;
        case 0x10 :
          aType = WRITE;
          break;
        case 0x20 :
          aType = NI_READ;
          break;
        case 0x30 :
          aType = NI_WRITE;
          break;
        case 0x40 :
          aType = RMW_BITS;
          break;
        case 0x50 :
          aType = RMW_SUM;
          break;
        default:
          log ( Error() , "Unknown IPbus-header " , Integer ( uint8_t ( ( aHeader & 0xF0 ) >>4 ) , IntFmt<hex,fixed>() ) );
          throw exception::ValidationError();
      }

      aWordCount = ( aHeader >> 8 ) & 0xff;
      aTransactionId = ( aHeader >> 16 ) & 0xfff;
      aInfoCode = aHeader & 0xf;
      return true;
    }
    catch ( const std::exception& aExc )
    {
      return false;
    }
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  uint32_t IPbus< 2 , IPbus_minor , buffer_size >::implementCalculateHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId )
  {
    return IPbus< 2 , IPbus_minor , buffer_size >::CalculateHeader ( aType , aWordCount , aTransactionId );
  }

  template< uint8_t IPbus_minor , uint32_t buffer_size >
  bool IPbus< 2 , IPbus_minor , buffer_size >::implementExtractHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aInfoCode )
  {
    return IPbus< 2 , IPbus_minor , buffer_size >::ExtractHeader ( aHeader , aType , aWordCount , aTransactionId , aInfoCode );
  }
  // --------------------------------------------------------------------------------------------------------------------------------------------------------------

}


