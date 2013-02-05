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

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

---------------------------------------------------------------------------
*/

#include "uhal/IPbusPacketInfo.hpp"
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>

using boost::asio::ip::udp;
using namespace uhal;

static const uint32_t ADDRESSMASK = 0x000FFFFF;

class UDPdummyHardware
{
  public:

    UDPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay ) :
      mIOservice(),
      mSocket ( mIOservice , udp::endpoint ( udp::v4(), aPort ) ),
      mMemory ( ADDRESSMASK+1 , 0x00000000 ),
      mReplyDelay ( aReplyDelay )
    {
      logging();
    }


    ~UDPdummyHardware()
    {
      logging();
    }

    void run()
    {
      logging();

      for ( ;; )
      {
        uint32_t lUDPreceiveCounter = mSocket.receive_from ( boost::asio::buffer ( mUDPreceiveBuffer, 500<<2 ) , mSenderEndpoint );
        uint32_t* lReceivePtr ( mUDPreceiveBuffer );
        uint32_t* lReceiveEnd ( lReceivePtr+ ( lUDPreceiveCounter>>2 ) );
        uint32_t* lReplyPtr ( mUDPreplyBuffer );

        do
        {
          if ( ! IPbusHeaderHelper< 1,3 >::extract (
                 *lReceivePtr ,
                 mType ,
                 mWordCounter ,
                 mTransactionId ,
                 mResponseGood )
             )
          {
            log ( Error() , "Unable to parse send header " , Integer ( *lReceivePtr, IntFmt<hex,fixed>() ) );
            return;
          }

          lReceivePtr++;

          switch ( mType )
          {
            case B_O_T:
              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
              lReplyPtr++;
              break;
            case R_A_I:
              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , 2 , mTransactionId ) | 0x4;
              lReplyPtr+=3;
              break;
            case NI_READ:
              mAddress = *lReceivePtr;
              lReceivePtr++;
              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
              lReplyPtr++;

              for ( ; mWordCounter!=0 ; --mWordCounter )
              {
                *lReplyPtr = mMemory.at ( mAddress & ADDRESSMASK );
                lReplyPtr++;
              }

              break;
            case READ:
              mAddress = *lReceivePtr;
              lReceivePtr++;
              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
              lReplyPtr++;

              for ( ; mWordCounter!=0 ; --mWordCounter )
              {
                *lReplyPtr = mMemory.at ( mAddress++ & ADDRESSMASK );
                lReplyPtr++;
              }

              break;
            case NI_WRITE:
              mAddress = *lReceivePtr;
              lReceivePtr++;

              for ( ; mWordCounter!=0 ; --mWordCounter )
              {
                mMemory.at ( mAddress & ADDRESSMASK ) = *lReceivePtr;
                lReceivePtr++;
              }

              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
              lReplyPtr++;
              break;
            case WRITE:
              mAddress = *lReceivePtr;
              lReceivePtr++;

              for ( ; mWordCounter!=0 ; --mWordCounter )
              {
                mMemory.at ( mAddress++ & ADDRESSMASK ) = *lReceivePtr;
                lReceivePtr++;
              }

              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
              lReplyPtr++;
              break;
            case RMW_SUM:
              mAddress = *lReceivePtr;
              lReceivePtr++;
              mMemory.at ( mAddress & ADDRESSMASK ) += ( int32_t ) ( *lReceivePtr );
              lReceivePtr++;
              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , 1 , mTransactionId ) | 0x4;
              lReplyPtr++;
              *lReplyPtr = mMemory.at ( mAddress & ADDRESSMASK );
              lReplyPtr++;
              break;
            case RMW_BITS:
              mAddress = *lReceivePtr;
              lReceivePtr++;
              mMemory.at ( mAddress & ADDRESSMASK ) &= ( int32_t ) ( *lReceivePtr );
              lReceivePtr++;
              mMemory.at ( mAddress & ADDRESSMASK ) |= ( int32_t ) ( *lReceivePtr );
              lReceivePtr++;
              *lReplyPtr = IPbusHeaderHelper< 1 , 3 >::calculate ( mType , 1 , mTransactionId ) | 0x4;
              lReplyPtr++;
              *lReplyPtr = mMemory.at ( mAddress & ADDRESSMASK );
              lReplyPtr++;
              break;
          }
        }
        while ( lReceivePtr!=lReceiveEnd );

        if ( mReplyDelay )
        {
          log ( Info() , "Sleeping for " , Integer ( mReplyDelay ) , "s" );
          sleep ( mReplyDelay );
          mReplyDelay = 0;
          log ( Info() , "Now replying " );
        }

        mSocket.send_to ( boost::asio::buffer ( mUDPreplyBuffer , ( lReplyPtr-mUDPreplyBuffer ) <<2 ) , mSenderEndpoint );
      }
    }

  private:
    boost::asio::io_service mIOservice;
    udp::socket mSocket;
    udp::endpoint mSenderEndpoint;

    std::vector< uint32_t > mMemory;

    uint32_t mUDPreceiveBuffer[500];
    uint32_t mUDPreplyBuffer[500];

    eIPbusTransactionType mType;
    uint32_t mWordCounter;
    uint32_t mTransactionId;
    uint8_t mResponseGood;

    uint32_t mAddress;

    uint32_t mReplyDelay;

};



int main ( int argc, char* argv[] )
{
  logging();

  if ( argc < 2 || argc > 3 )
  {
    log ( Error() , "Usage: " , ( const char* ) ( argv[0] ) , " <port> <optional reply delay for first packet in seconds>" );
    return 1;
  }

  uint32_t lReplyDelay ( 0 );

  if ( argc == 3 )
  {
    lReplyDelay = boost::lexical_cast<uint16_t> ( argv[2] );
  }

  UDPdummyHardware lDummyHardware ( boost::lexical_cast<uint16_t> ( argv[1] ) , lReplyDelay );
  lDummyHardware.run();
  return 0;
}
