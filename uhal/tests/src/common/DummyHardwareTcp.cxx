#include "uhal/IPbusPacketInfo.hpp"
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>

using boost::asio::ip::tcp;
using namespace uhal;

static const uint32_t ADDRESSMASK = 0x000FFFFF;


class TCPdummyHardware
{
  public:

  TCPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay ) try :
      mIOservice(),
                 mAcceptor ( mIOservice , tcp::endpoint ( tcp::v4() , aPort ) ),
                 mSocket ( mIOservice ),
                 mMemory ( uint32_t ( ADDRESSMASK+1 ) , 0x00000000 ),
                 mReplyDelay ( aReplyDelay )
    {
      log ( Info() , "Assigned " , Integer ( uint32_t ( ADDRESSMASK+1 ) ) , " words of memory" );
      mAcceptor.accept ( mSocket );
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }

    ~TCPdummyHardware() {}

    void run()
    {
      try
      {
        for ( ;; )
        {
          boost::system::error_code lError;
          uint32_t lTCPreceiveCounter = mSocket.read_some ( boost::asio::buffer ( mTCPreceiveBuffer, 500<<2 ) , lError );
          //log( Info() , "Read " , Integer ( lTCPreceiveCounter ) );

          if ( lError == boost::asio::error::eof )
          {
            //log( Info() , "Got error code eof" );
            //mSocket.close();
            //mAcceptor.accept ( mSocket );
            //continue;
            break; // Connection closed cleanly by peer.
          }

          uint32_t* lReceivePtr ( mTCPreceiveBuffer );
          uint32_t* lReceiveEnd ( lReceivePtr+ ( lTCPreceiveCounter>>2 ) );
          uint32_t* lReplyPtr ( mTCPreplyBuffer );

          do
          {
            log ( Info() , "Header = " , Integer ( *lReceivePtr, IntFmt<hex,fixed>() ) );

            if ( ! IPbusHeaderHelper< IPbus_1_3 >::extract (
                   *lReceivePtr ,
                   mType ,
                   mWordCounter ,
                   mTransactionId ,
                   mResponseGood )
               )
            {
              log ( Error() , "Unable to parse send header " ,  Integer ( *lReceivePtr, IntFmt<hex,fixed>() ) );
              return;
            }

            lReceivePtr++;
            log ( Info() , " - mType = " , Integer ( uint32_t ( mType ) ) );
            log ( Info() , " - mWordCounter = " , Integer ( uint32_t ( mWordCounter ) ) );
            log ( Info() , " - mTransactionId = " , Integer ( uint32_t ( mTransactionId ) ) );

            switch ( mType )
            {
              case B_O_T:
                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
                lReplyPtr++;
                break;
              case R_A_I:
                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 2 , mTransactionId ) | 0x4;
                lReplyPtr+=3;
                break;
              case NI_READ:
                mAddress = *lReceivePtr;
                lReceivePtr++;
                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
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
                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
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

                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
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

                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
                lReplyPtr++;
                break;
              case RMW_SUM:
                mAddress = *lReceivePtr;
                lReceivePtr++;
                mMemory.at ( mAddress & ADDRESSMASK ) += ( int32_t ) ( *lReceivePtr );
                lReceivePtr++;
                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 1 , mTransactionId ) | 0x4;
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
                *lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 1 , mTransactionId ) | 0x4;
                lReplyPtr++;
                *lReplyPtr = mMemory.at ( mAddress & ADDRESSMASK );
                lReplyPtr++;
                break;
            }
          }
          while ( lReceivePtr!=lReceiveEnd );

          sleep ( mReplyDelay );
          mReplyDelay = 0;
          boost::asio::write ( mSocket , boost::asio::buffer ( mTCPreplyBuffer , ( lReplyPtr-mTCPreplyBuffer ) <<2 ) );
        }
      }
      catch ( uhal::exception& aExc )
      {
        aExc.rethrowFrom ( ThisLocation() );
      }
      catch ( const std::exception& aExc )
      {
        StdException ( aExc ).throwFrom ( ThisLocation() );
      }
    }

  private:
    boost::asio::io_service mIOservice;
    tcp::acceptor mAcceptor;
    tcp::socket mSocket;
    tcp::endpoint mSenderEndpoint;

    std::vector< uint32_t > mMemory;

    uint32_t mTCPreceiveBuffer[500];
    uint32_t mTCPreplyBuffer[500];

    eIPbusTransactionType mType;
    uint32_t mWordCounter;
    uint32_t mTransactionId;
    uint8_t mResponseGood;

    uint32_t mAddress;

    uint32_t mReplyDelay;

};



int main ( int argc, char* argv[] )
{
  try
  {
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

    while ( true )
    {
      TCPdummyHardware lDummyHardware ( boost::lexical_cast<uint16_t> ( argv[1] ) , lReplyDelay );
      lDummyHardware.run();
    }
  }
  catch ( uhal::exception& aExc )
  {
    aExc.rethrowFrom ( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  {
    StdException ( aExc ).throwFrom ( ThisLocation() );
  }

  return 0;
}
