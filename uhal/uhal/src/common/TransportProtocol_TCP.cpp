// #include "uhal/performance.hpp"
#include "uhal/TransportProtocol_TCP.hpp"

// #include <boost/lambda/bind.hpp>
#include <boost/bind/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>


#include <sys/time.h>

namespace uhal
{


TcpTransportProtocol::DispatchWorker::DispatchWorker ( TcpTransportProtocol& aTcpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , const boost::posix_time::time_duration& aTimeoutPeriod ) try :
    mTcpTransportProtocol ( aTcpTransportProtocol ),
                          mIOservice ( boost::shared_ptr< boost::asio::io_service > ( new boost::asio::io_service() ) ),
                          mSocket ( boost::shared_ptr< boost::asio::ip::tcp::socket > ( new boost::asio::ip::tcp::socket ( *mIOservice ) ) ),
                          mEndpoint ( boost::shared_ptr< boost::asio::ip::tcp::resolver::iterator > (
                                        new boost::asio::ip::tcp::resolver::iterator (
                                          boost::asio::ip::tcp::resolver ( *mIOservice ).resolve (
                                            boost::asio::ip::tcp::resolver::query ( aHostname , aServiceOrPort )
                                          )
                                        )
                                      )
                                    ),
                          mDeadlineTimer ( *mIOservice ),
                          mTimeoutPeriod ( aTimeoutPeriod ),
                          mReplyMemory ( 65536 , 0x00000000 )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TcpTransportProtocol::DispatchWorker::CheckDeadline, this ) );
  }
  catch ( uhal::exception& aExc )
  {
    aExc.rethrowFrom ( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  {
    StdException ( aExc ).throwFrom ( ThisLocation() );
  }


  TcpTransportProtocol::DispatchWorker::~DispatchWorker()
  {
    try
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }


  void TcpTransportProtocol::DispatchWorker::operator() ()
  {
#ifdef USE_TCP_MULTITHREADED

    try
    {
      bool lBuffersPending ( false );
      Buffers* lBuffers ( NULL );

      while ( true )
      {
        boost::this_thread::interruption_point();
        {
          boost::lock_guard<boost::mutex> lLock ( mTcpTransportProtocol.mMutex );
          lBuffersPending = ( mTcpTransportProtocol.mPendingSendBuffers.size() != 0 );
        }

        if ( lBuffersPending )
        {
          {
            boost::lock_guard<boost::mutex> lLock ( mTcpTransportProtocol.mMutex );
            lBuffers = mTcpTransportProtocol.mPendingSendBuffers.front();
          }
          Dispatch ( lBuffers );
          {
            boost::lock_guard<boost::mutex> lLock ( mTcpTransportProtocol.mMutex );
            mTcpTransportProtocol.mPendingSendBuffers.pop_front();
          }
        }
      }
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
      mTcpTransportProtocol.mAsynchronousException = new uhal::exception ( aExc );
    }
    catch ( boost::thread_interrupted& )
    {
      //This thread was interrupted by main thread, which is ok - we just exit
    }

#endif
  }






  void TcpTransportProtocol::DispatchWorker::Dispatch ( Buffers* aBuffers )
  {
    try
    {
      if ( ! mSocket->is_open() )
      {
        log ( Info() , "Attempting to create TCP connection to '" , ( **mEndpoint ).host_name() , "' port " , ( **mEndpoint ).service_name() , "." );
        boost::asio::connect ( *mSocket , *mEndpoint );
        mSocket->set_option ( boost::asio::ip::tcp::no_delay ( true ) );
        log ( Info() , "TCP connection succeeded" );
      }

      // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
      // Send data
      // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
      std::vector< boost::asio::const_buffer > lAsioSendBuffer;
      lAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
      log ( Debug() , "Sending " , Integer ( aBuffers->sendCounter() ) , " bytes" );
      mDeadlineTimer.expires_from_now ( mTimeoutPeriod );
      mErrorCode = boost::asio::error::would_block;
      boost::asio::async_write ( *mSocket , lAsioSendBuffer , boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

      do
      {
        mIOservice->run_one();
      }
      while ( mErrorCode == boost::asio::error::would_block );

      if ( mErrorCode )
      {
        log ( Error() , "ASIO reported an error: " , mErrorCode.message() );
        ErrorInTcpCallback().throwFrom ( ThisLocation() );
      }

      // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
      // Read back replies
      // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
      std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
      lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , aBuffers->replyCounter() ) );
      log ( Debug() , "Expecting " , Integer ( aBuffers->replyCounter() ) , " bytes in reply" );
      mDeadlineTimer.expires_from_now ( mTimeoutPeriod );
      mErrorCode = boost::asio::error::would_block;
      boost::asio::async_read ( *mSocket , lAsioReplyBuffer ,  boost::asio::transfer_all(), boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

      do
      {
        mIOservice->run_one();
      }
      while ( mErrorCode == boost::asio::error::would_block );

      if ( mErrorCode )
      {
        log ( Error() , "ASIO reported an error: " , Quote ( mErrorCode.message() ) , ". Attempting validation to see if we can get any more info." );
        mTcpTransportProtocol.mPackingProtocol->Validate ( aBuffers );
        ErrorInTcpCallback().throwFrom ( ThisLocation() );
      }

      std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( aBuffers->getReplyBuffer() );
      uint8_t* lReplyBuf ( & ( mReplyMemory.at ( 0 ) ) );

      for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
      {
        memcpy ( lIt->first, lReplyBuf, lIt->second );
        lReplyBuf += lIt->second;
      }

      /*
      			uint32_t lCounter(0);
      			for ( std::vector< boost::asio::mutable_buffer >::iterator lIt = lAsioReplyBuffer.begin() ; lIt != lAsioReplyBuffer.end() ; ++lIt )
      			{
      				uint32_t s1 = boost::asio::buffer_size(*lIt)>>2;
      				uint32_t* p1 = boost::asio::buffer_cast<uint32_t*>(*lIt);

      				for( uint32_t i(0) ; i!= s1 ; ++i , ++p1 )
      				{
      					log ( Debug() , Integer ( lCounter++ ) , " : " , Integer ( *p1 , IntFmt<hex,fixed>() ) );
      				}
      			}
      */

      if ( !mTcpTransportProtocol.mPackingProtocol->Validate ( aBuffers ) )
      {
        log ( Error() , "Validation function reported an error!" );
        IPbusValidationError ().throwFrom ( ThisLocation() );
      }
    }
    catch ( uhal::exception& aExc )
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }

      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }

      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }



  void TcpTransportProtocol::DispatchWorker::CheckDeadline()
  {
    // Check whether the deadline has passed. We compare the deadline against
    // the current time since a new asynchronous operation may have moved the
    // deadline before this actor had a chance to run.
    if ( mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
    {
      // The deadline has passed. The socket is closed so that any outstanding
      // asynchronous operations are cancelled.
      mSocket->close();
      // There is no longer an active deadline. The expiry is set to positive
      // infinity so that the actor takes no action until a new deadline is set.
      mDeadlineTimer.expires_at ( boost::posix_time::pos_infin );
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ( boost::bind ( &TcpTransportProtocol::DispatchWorker::CheckDeadline, this ) );
  }



TcpTransportProtocol::TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , const boost::posix_time::time_duration& aTimeoutPeriod ) try :
    TransportProtocol ( aTimeoutPeriod ),
                      mDispatchWorker ( boost::shared_ptr< DispatchWorker > ( new DispatchWorker ( *this , aHostname , aServiceOrPort , aTimeoutPeriod ) ) )
#ifdef USE_TCP_MULTITHREADED
                      , mDispatchThread ( boost::shared_ptr< boost::thread > ( new boost::thread ( *mDispatchWorker ) ) ),
                      mAsynchronousException ( NULL )
#endif
    {}
  catch ( uhal::exception& aExc )
  {
    aExc.rethrowFrom ( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  {
    StdException ( aExc ).throwFrom ( ThisLocation() );
  }


  TcpTransportProtocol::~TcpTransportProtocol()
  {
    try
    {
#ifdef USE_TCP_MULTITHREADED

      if ( mDispatchThread.unique() )
      {
        mDispatchThread->interrupt();
        //For some reason a call to join blocks when placed in the destructor (see Google) so we just let the thread detach as the Transport Protocol should only be killed at the end of the program anyway...
        //mDispatchThread->join();
      }

      {
        boost::lock_guard<boost::mutex> lLock ( mMutex );

        if ( mAsynchronousException )
        {
          delete mAsynchronousException;
          mAsynchronousException = NULL;
        }
      }

#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }




  void TcpTransportProtocol::Dispatch ( Buffers* aBuffers )
  {
    try
    {
#ifdef USE_TCP_MULTITHREADED
      {
        boost::lock_guard<boost::mutex> lLock ( mMutex );

        if ( mAsynchronousException )
        {
          mAsynchronousException->throwFrom ( ThisLocation() );
        }

        mPendingSendBuffers.push_back ( aBuffers );

      }
#else
      mDispatchWorker->Dispatch ( aBuffers );
#endif
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



  void TcpTransportProtocol::Flush( )
  {
    try
    {
#ifdef USE_TCP_MULTITHREADED
      bool lContinue ( true );

      do
      {
        boost::lock_guard<boost::mutex> lLock ( mMutex );

        // kill time while pending buffers are emptied and check for exceptions on the thread...
        if ( mAsynchronousException )
        {
          mAsynchronousException->throwFrom ( ThisLocation() );
        }

        lContinue = ( mPendingSendBuffers.size() );
      }
      while ( lContinue );

#endif
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




}

