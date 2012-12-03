#include "uhal/IPbusPacketInfo.hpp"
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/thread/mutex.hpp>

using boost::asio::ip::tcp;
using namespace uhal;

static const uint32_t ADDRESSMASK = 0x000FFFFF;

class TCPdummyHardware;


class TCPdummyHardwareSession
{
    friend class TCPdummyHardware;

  public:
    TCPdummyHardwareSession ( TCPdummyHardware& aTCPdummyHardware );

    ~TCPdummyHardwareSession();

    void start();

  private:
    void handle_read ( const boost::system::error_code& error, size_t bytes_transferred );

    void handle_write ( const boost::system::error_code& error );

    TCPdummyHardware& mTCPdummyHardware;
    tcp::socket mSocket;

    uint32_t* mReceivePtr;
    uint32_t* mReplyPtr;

    uint32_t mTCPreceiveBuffer[500];
    uint32_t mTCPreplyBuffer[500];

    eIPbusTransactionType mType;
    uint32_t mWordCounter;
    uint32_t mTransactionId;
    uint8_t mResponseGood;

    uint32_t mAddress;

};




class TCPdummyHardware
{
    friend class TCPdummyHardwareSession;

  public:

    TCPdummyHardware ( boost::asio::io_service& aIOservice , const uint16_t& aPort , const uint32_t& aReplyDelay );

    ~TCPdummyHardware();

  private:
    void start_accept();

    void handle_accept ( TCPdummyHardwareSession* aSession, const boost::system::error_code& error );

  private:
    boost::asio::io_service& mIOservice;

    tcp::acceptor mAcceptor;
    tcp::endpoint mSenderEndpoint;

    std::vector< uint32_t > mMemory;

    boost::mutex mMutex;

    uint32_t mReplyDelay;

};



// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




TCPdummyHardware::TCPdummyHardware ( boost::asio::io_service& aIOservice , const uint16_t& aPort , const uint32_t& aReplyDelay ) try :
  mIOservice ( aIOservice ),
             mAcceptor ( aIOservice , tcp::endpoint ( tcp::v4() , aPort ) ),
             mMemory ( uint32_t ( ADDRESSMASK+1 ) , 0x00000000 ),
             mReplyDelay ( aReplyDelay )
{
  //      log ( Info() , "Assigned " , Integer ( uint32_t ( ADDRESSMASK+1 ) ) , " words of memory" );
  start_accept();
}
catch ( uhal::exception& aExc )
{
  aExc.rethrowFrom ( ThisLocation() );
}
catch ( const std::exception& aExc )
{
  StdException ( aExc ).throwFrom ( ThisLocation() );
}

TCPdummyHardware::~TCPdummyHardware() {    }


void TCPdummyHardware::start_accept()
{
  TCPdummyHardwareSession* lSession = new TCPdummyHardwareSession ( *this );
  mAcceptor.async_accept (	lSession->mSocket ,
                            boost::bind (	&TCPdummyHardware::handle_accept,
                                          this,
                                          lSession,
                                          boost::asio::placeholders::error ) );
}

void TCPdummyHardware::handle_accept ( TCPdummyHardwareSession* aSession, const boost::system::error_code& error )
{
  if ( !error )
  {
    aSession->start();
  }
  else
  {
    delete aSession;
  }

  start_accept();
}



// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




TCPdummyHardwareSession::TCPdummyHardwareSession ( TCPdummyHardware& aTCPdummyHardware ) try :
  mTCPdummyHardware ( aTCPdummyHardware ),
                    mSocket ( aTCPdummyHardware.mIOservice ),
                    mReceivePtr ( mTCPreceiveBuffer ),
                    mReplyPtr ( mTCPreplyBuffer )
{
}
catch ( uhal::exception& aExc )
{
  aExc.rethrowFrom ( ThisLocation() );
}
catch ( const std::exception& aExc )
{
  StdException ( aExc ).throwFrom ( ThisLocation() );
}

TCPdummyHardwareSession::~TCPdummyHardwareSession() {    }

void TCPdummyHardwareSession::start()
{
  mSocket.async_read_some ( boost::asio::buffer ( mTCPreceiveBuffer, 500<<2 ),
                            boost::bind ( &TCPdummyHardwareSession::handle_read, this,
                                          boost::asio::placeholders::error,
                                          boost::asio::placeholders::bytes_transferred ) );
}


void TCPdummyHardwareSession::handle_read ( const boost::system::error_code& error,
    size_t bytes_transferred )
{
  if ( !error )
  {
    boost::lock_guard<boost::mutex> lLock ( mTCPdummyHardware.mMutex );
    uint32_t* lReceiveEnd ( mReceivePtr+ ( bytes_transferred>>2 ) );

    do
    {
      //log ( Info() , "Header = " , Integer ( *mReceivePtr, IntFmt<hex,fixed>() ) );
      if ( ! IPbusHeaderHelper< IPbus_1_3 >::extract (
             *mReceivePtr ,
             mType ,
             mWordCounter ,
             mTransactionId ,
             mResponseGood )
         )
      {
        log ( Error() , "Unable to parse send header " ,  Integer ( *mReceivePtr, IntFmt<hex,fixed>() ) );
        return;
      }

      mReceivePtr++;
      //log ( Info() , " - mType = " , Integer ( uint32_t ( mType ) ) );
      //log ( Info() , " - mWordCounter = " , Integer ( uint32_t ( mWordCounter ) ) );
      //log ( Info() , " - mTransactionId = " , Integer ( uint32_t ( mTransactionId ) ) );

      switch ( mType )
      {
        case B_O_T:
          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
          mReplyPtr++;
          break;
        case R_A_I:
          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 2 , mTransactionId ) | 0x4;
          mReplyPtr+=3;
          break;
        case NI_READ:
          mAddress = *mReceivePtr;
          mReceivePtr++;
          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
          mReplyPtr++;

          for ( ; mWordCounter!=0 ; --mWordCounter )
          {
            *mReplyPtr = mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK );
            mReplyPtr++;
          }

          break;
        case READ:
          mAddress = *mReceivePtr;
          mReceivePtr++;
          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
          mReplyPtr++;

          for ( ; mWordCounter!=0 ; --mWordCounter )
          {
            *mReplyPtr = mTCPdummyHardware.mMemory.at ( mAddress++ & ADDRESSMASK );
            mReplyPtr++;
          }

          break;
        case NI_WRITE:
          mAddress = *mReceivePtr;
          mReceivePtr++;

          for ( ; mWordCounter!=0 ; --mWordCounter )
          {
            mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK ) = *mReceivePtr;
            mReceivePtr++;
          }

          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
          mReplyPtr++;
          break;
        case WRITE:
          mAddress = *mReceivePtr;
          mReceivePtr++;

          for ( ; mWordCounter!=0 ; --mWordCounter )
          {
            mTCPdummyHardware.mMemory.at ( mAddress++ & ADDRESSMASK ) = *mReceivePtr;
            mReceivePtr++;
          }

          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
          mReplyPtr++;
          break;
        case RMW_SUM:
          mAddress = *mReceivePtr;
          mReceivePtr++;
          mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK ) += ( int32_t ) ( *mReceivePtr );
          mReceivePtr++;
          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 1 , mTransactionId ) | 0x4;
          mReplyPtr++;
          *mReplyPtr = mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK );
          mReplyPtr++;
          break;
        case RMW_BITS:
          mAddress = *mReceivePtr;
          mReceivePtr++;
          mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK ) &= ( int32_t ) ( *mReceivePtr );
          mReceivePtr++;
          mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK ) |= ( int32_t ) ( *mReceivePtr );
          mReceivePtr++;
          *mReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 1 , mTransactionId ) | 0x4;
          mReplyPtr++;
          *mReplyPtr = mTCPdummyHardware.mMemory.at ( mAddress & ADDRESSMASK );
          mReplyPtr++;
          break;
      }
    }
    while ( mReceivePtr!=lReceiveEnd );

    //Think I will need to check here to see if the TCP packet has been broken up...
    sleep ( mTCPdummyHardware.mReplyDelay );
    mTCPdummyHardware.mReplyDelay = 0;
    boost::asio::async_write ( mSocket,
                               boost::asio::buffer ( mTCPreplyBuffer , ( mReplyPtr-mTCPreplyBuffer ) <<2 ),
                               boost::bind ( &TCPdummyHardwareSession::handle_write, this,
                                   boost::asio::placeholders::error ) );
    mReceivePtr = mTCPreceiveBuffer;
    mReplyPtr = mTCPreplyBuffer;
  }
  else
  {
    delete this;
  }
}

void TCPdummyHardwareSession::handle_write ( const boost::system::error_code& error )
{
  if ( !error )
  {
    mSocket.async_read_some ( boost::asio::buffer ( mTCPreceiveBuffer, 500<<2 ),
                              boost::bind ( &TCPdummyHardwareSession::handle_read, this,
                                            boost::asio::placeholders::error,
                                            boost::asio::placeholders::bytes_transferred ) );
  }
  else
  {
    delete this;
  }
}





// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





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

    boost::asio::io_service lIOservice;
    TCPdummyHardware lDummyHardware ( lIOservice , boost::lexical_cast<uint16_t> ( argv[1] ) , lReplyDelay );
    lIOservice.run();
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


