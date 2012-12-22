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


  TcpTransportProtocol::DispatchWorker::DispatchWorker ( TcpTransportProtocol& aTcpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort ) :
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
    mReplyMemory ( 65536 , 0x00000000 )
  {
    logging();
    mDeadlineTimer.async_wait ( boost::bind ( &TcpTransportProtocol::DispatchWorker::CheckDeadline, this ) );
  }



  TcpTransportProtocol::DispatchWorker::~DispatchWorker()
  {
    logging();

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
    logging();

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
    logging();
    log ( Info() , ThisLocation() , " : mTimeOut = " , Integer ( mTcpTransportProtocol.getTimeoutPeriod().total_milliseconds() ) );

    if ( ! mSocket->is_open() )
    {
      log ( Info() , "Attempting to create TCP connection to '" , ( **mEndpoint ).host_name() , "' port " , ( **mEndpoint ).service_name() , "." );
      mDeadlineTimer.expires_from_now ( mTcpTransportProtocol.getTimeoutPeriod() );
      mErrorCode = boost::asio::error::would_block;
      boost::asio::async_connect ( *mSocket , *mEndpoint , boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

      do
      {
        mIOservice->run_one();
      }
      while ( mErrorCode == boost::asio::error::would_block );

      if ( mErrorCode )
      {
        if ( mSocket.unique() )
        {
          mSocket->close();
        }

        mTcpTransportProtocol.mPackingProtocol->DeleteBuffer();

        if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
        {
          log ( Error() , "ASIO reported a Timeout in TCP callback" );
          throw ErrorInTcpCallback();
        }

        log ( Error() , "ASIO reported an error: " , mErrorCode.message() );
        throw ErrorInTcpCallback();
      }

      mSocket->set_option ( boost::asio::ip::tcp::no_delay ( true ) );
      log ( Info() , "TCP connection succeeded" );
    }

    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Send data
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( aBuffers->sendCounter() ) , " bytes" );
    mDeadlineTimer.expires_from_now ( mTcpTransportProtocol.getTimeoutPeriod() );
    mErrorCode = boost::asio::error::would_block;
    boost::asio::async_write ( *mSocket , lAsioSendBuffer , boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice->run_one();
    }
    while ( mErrorCode == boost::asio::error::would_block );

    /*if ( mErrorCode )
      {
        log ( Error() , "ASIO reported an error: " , mErrorCode.message() );
        throw ErrorInTcpCallback();
      }*/
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Read back replies
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , aBuffers->replyCounter() ) );
    log ( Debug() , "Expecting " , Integer ( aBuffers->replyCounter() ) , " bytes in reply" );
    mDeadlineTimer.expires_from_now ( mTcpTransportProtocol.getTimeoutPeriod() );
    mErrorCode = boost::asio::error::would_block;
    boost::asio::async_read ( *mSocket , lAsioReplyBuffer ,  boost::asio::transfer_all(), boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice->run_one();
    }
    while ( mErrorCode == boost::asio::error::would_block );

    if ( mErrorCode && ( mErrorCode != boost::asio::error::eof ) )
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }

      mTcpTransportProtocol.mPackingProtocol->DeleteBuffer();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO reported a Timeout in TCP callback" );
        throw ErrorInTcpCallback();
      }

      log ( Error() , "ASIO reported an error: " , Quote ( mErrorCode.message() ) , ". Attempting validation to see if we can get any more info." );
      mTcpTransportProtocol.mPackingProtocol->Validate ( aBuffers );
      throw ErrorInTcpCallback();
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
      throw IPbusValidationError ();
    }
  }

  void TcpTransportProtocol::DispatchWorker::CheckDeadline()
  {
    logging();

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
      //set the error code correctly
      //20/12/2012 - awr - wherever this is in the function, this appears to cause a race condition which results in the timeout recovery failing.
      //mErrorCode = boost::asio::error::timed_out;
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ( boost::bind ( &TcpTransportProtocol::DispatchWorker::CheckDeadline, this ) );
  }
  TcpTransportProtocol::TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , const boost::posix_time::time_duration& aTimeoutPeriod ) :
    TransportProtocol ( aTimeoutPeriod ),
    mDispatchWorker ( boost::shared_ptr< DispatchWorker > ( new DispatchWorker ( *this , aHostname , aServiceOrPort ) ) )
#ifdef USE_TCP_MULTITHREADED
    , mDispatchThread ( boost::shared_ptr< boost::thread > ( new boost::thread ( *mDispatchWorker ) ) ),
    mAsynchronousException ( NULL )
#endif
  {
    logging();
  }
  TcpTransportProtocol::~TcpTransportProtocol()
  {
    logging();

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
    logging();
#ifdef USE_TCP_MULTITHREADED
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );

      if ( mAsynchronousException )
      {
        throw mAsynchronousException-;
      }

      mPendingSendBuffers.push_back ( aBuffers );
    }
#else
    mDispatchWorker->Dispatch ( aBuffers );
#endif
  }
  void TcpTransportProtocol::Flush( )
  {
    logging();
#ifdef USE_TCP_MULTITHREADED
    bool lContinue ( true );

    do
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );

      // kill time while pending buffers are emptied and check for exceptions on the thread...
      if ( mAsynchronousException )
      {
        throw mAsynchronousException-;
      }

      lContinue = ( mPendingSendBuffers.size() );
    }
    while ( lContinue );

#endif
  }
}

