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
// #include <boost/lambda/bind.hpp>
#include <boost/bind/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>


#include <sys/time.h>

namespace uhal
{




  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol >
  UDP< InnerProtocol >::DispatchWorker::DispatchWorker ( UDP< InnerProtocol >& aUDP , const std::string& aHostname , const std::string& aServiceOrPort ) :
    mUDP ( aUDP ),
    mIOservice ( boost::shared_ptr< boost::asio::io_service > ( new boost::asio::io_service() ) ),
    mSocket ( boost::shared_ptr< boost::asio::ip::udp::socket > ( new boost::asio::ip::udp::socket ( *mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) ) ) ),
    mEndpoint ( boost::shared_ptr< boost::asio::ip::udp::endpoint > ( new boost::asio::ip::udp::endpoint (
                  * boost::asio::ip::udp::resolver::iterator (
                    boost::asio::ip::udp::resolver ( *mIOservice ).resolve (
                      boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , aHostname , aServiceOrPort )
                    )
                  )
                )
                                                                    )
              ),
    mDeadlineTimer ( *mIOservice ),
    mReplyMemory ( 65536 , 0x00000000 )
  {
    logging();
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::DispatchWorker::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  UDP< InnerProtocol >::DispatchWorker::~DispatchWorker()
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

  template < typename InnerProtocol >
  void UDP< InnerProtocol >::DispatchWorker::operator() ()
  {
#ifdef USE_UDP_MULTITHREADED
    logging();

    try
    {
      bool lBuffersPending ( false );
      Buffers* lBuffers ( NULL );

      while ( true )
      {
        boost::this_thread::interruption_point();
        Buffers* lBuffers ( mUDP.getCurrentDispatchBuffers() );

        if ( lBuffers )
        {
          mDispatchWorker->dispatch ( lBuffers );
        }
      }
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
      mUDP.mAsynchronousException = new uhal::exception ( aExc );
    }
    catch ( boost::thread_interrupted& )
    {
      //This thread was interrupted by main thread, which is ok - we just exit
    }

#endif
  }





  template < typename InnerProtocol >
  void UDP< InnerProtocol >::DispatchWorker::dispatch ( Buffers* aBuffers )
  {
    logging();

    if ( ! mSocket->is_open() )
    {
      log ( Info() , "Creating new UDP socket, as it appears to have been closed..." );
      mSocket = boost::shared_ptr< boost::asio::ip::udp::socket > ( new boost::asio::ip::udp::socket ( *mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) ) );
      log ( Info() , "UDP socket created successfully." );
    }

    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Send data
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( aBuffers->sendCounter() ) , " bytes" );
    mDeadlineTimer.expires_from_now ( mUDP.getRawTimeoutPeriod() );
    mErrorCode = boost::asio::error::would_block;
    mSocket->async_send_to ( lAsioSendBuffer , *mEndpoint , boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice->run_one();
    }
    while ( mErrorCode == boost::asio::error::would_block );

    /*if ( mErrorCode )
    {
    log ( Error() , "ASIO reported an error: " , mErrorCode.message() );
    throw exception::ErrorInUdpCallback();
    }*/
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Read back replies
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , aBuffers->replyCounter() ) );
    log ( Debug() , "Expecting " , Integer ( aBuffers->replyCounter() ) , " bytes in reply" );
    boost::asio::ip::udp::endpoint lEndpoint;
    mDeadlineTimer.expires_from_now ( mUDP.getRawTimeoutPeriod() );
    mErrorCode = boost::asio::error::would_block;
    mSocket->async_receive_from ( lAsioReplyBuffer , lEndpoint , 0 , boost::lambda::var ( mErrorCode ) = boost::lambda::_1 );

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

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO reported a Timeout in UDP callback" );
        throw exception::ErrorInUdpCallback();
      }

      log ( Error() , "ASIO reported an error: " , Quote ( mErrorCode.message() ) , ". Attempting validation to see if we can get any more info." );
      mUDP.validate();
      throw exception::ErrorInUdpCallback();
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

    if ( !mUDP.validate() )
    {
      log ( Error() , "Validation function reported an error!" );
      throw exception::ValidationError ();
    }
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::DispatchWorker::CheckDeadline()
  {
    logging();
    //log ( Debug() , ThisLocation() );

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
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::DispatchWorker::CheckDeadline, this ) );
  }


  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol >
  UDP< InnerProtocol >::UDP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mDispatchWorker ( boost::shared_ptr< DispatchWorker > ( new DispatchWorker ( *this , aUri.mHostname , aUri.mPort ) ) )
#ifdef USE_UDP_MULTITHREADED
    , mDispatchThread ( boost::shared_ptr< boost::thread > ( new boost::thread ( *mDispatchWorker ) ) ),
    mAsynchronousException ( NULL )
#endif
  {
    logging();
    //log ( Debug() , ThisLocation() );
  }


  template < typename InnerProtocol >
  UDP< InnerProtocol >::~UDP()
  {
    logging();
    //log ( Debug() , ThisLocation() );

    try
    {
#ifdef USE_UDP_MULTITHREADED

      if ( mDispatchThread.unique() )
      {
        mDispatchThread->interrupt();
        //For some reason a call to join blocks when placed in the destructor (see Google) so we just let the thread detach as the Transport Protocol should only be killed at the end of the program anyway...
        //mDispatchThread->join();
      }

      if ( mAsynchronousException )
      {
        delete mAsynchronousException;
        mAsynchronousException = NULL;
      }

#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::implementDispatch()
  {
    logging();
    //log ( Debug() , ThisLocation() );
#ifndef USE_UDP_MULTITHREADED
    //log ( Debug() , ThisLocation() );
    Buffers* lBuffers ( getCurrentDispatchBuffers() );
    //log ( Debug() , ThisLocation() );

    if ( lBuffers )
    {
      //log ( Debug() , ThisLocation() );
      mDispatchWorker->dispatch ( lBuffers );
    }

    //log ( Debug() , ThisLocation() );
#endif
  }


  template < typename InnerProtocol >
  Buffers* UDP< InnerProtocol >::getCurrentDispatchBuffers()
  {
    logging();
    //log ( Debug() , ThisLocation() );

    if ( this->mCurrentDispatchBuffers == this->mCurrentFillingBuffers )
    {
      return NULL;
    }
    else
    {
      return this->mCurrentDispatchBuffers;
    }
  }

  template < typename InnerProtocol >
  const boost::posix_time::time_duration& UDP< InnerProtocol >::getRawTimeoutPeriod()
  {
    return this->mTimeoutPeriod;
  }


  template < typename InnerProtocol >
  bool UDP< InnerProtocol >::validate()
  {
    return ClientInterface::validate();
  }

  // template < typename InnerProtocol >
  // void UDP< InnerProtocol >::Flush( )
  // {
  // logging();
  // #ifdef USE_UDP_MULTITHREADED
  // bool lContinue ( true );

  // do
  // {
  // boost::lock_guard<boost::mutex> lLock ( mMutex );

  // // kill time while pending buffers are emptied and check for exceptions on the thread...
  // if ( mAsynchronousException )
  // {
  // throw mAsynchronousException-;
  // }

  // lContinue = ( mPendingSendBuffers.size() );
  // }
  // while ( lContinue );

  // #endif
  // }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

