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

#include <boost/bind/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>


#include "uhal/IPbusInspector.hpp"


#include <sys/time.h>

namespace uhal
{














  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol >
  TCP< InnerProtocol >::TCP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mIOservice ( boost::shared_ptr< boost::asio::io_service > ( new boost::asio::io_service() ) ),
    mSocket ( boost::shared_ptr< boost::asio::ip::tcp::socket > ( new boost::asio::ip::tcp::socket ( *mIOservice ) ) ),
    mEndpoint ( boost::shared_ptr< boost::asio::ip::tcp::resolver::iterator > (
                  new boost::asio::ip::tcp::resolver::iterator (
                    boost::asio::ip::tcp::resolver ( *mIOservice ).resolve (
                      boost::asio::ip::tcp::resolver::query ( aUri.mHostname , aUri.mPort )
                    )
                  )
                )
              ),
    mDeadlineTimer ( *mIOservice ),
    mReplyMemory ( 65536 , 0x00000000 ),
    mDispatchThread ( boost::shared_ptr< boost::thread > ( new boost::thread ( boost::bind ( &boost::asio::io_service::run , & ( *mIOservice ) ) ) ) )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }



  template < typename InnerProtocol >
  TCP< InnerProtocol >::~TCP()
  {
    try
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }

      mIOservice->stop();
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::implementDispatch()
  {
    try
    {
      if ( ! mSocket->is_open() )
      {
        connect();
      }

      // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
      // Send data
      // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
      Buffers* lCurrentBuffer ( & ( * ( this->mCurrentBuffers ) ) );
#ifdef FORCE_ONE_ASYNC_OPERATION_AT_A_TIME
      boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
      mDispatchQueue.push_back ( lCurrentBuffer );

      if ( mDispatchQueue.size() == 1 )
      {
        write ( lCurrentBuffer );
      }

#else
      write ( lCurrentBuffer );
#endif
      std::cout << '.' << std::flush;
    }
    catch ( const std::exception& aExc )
    {
      this->dispatchExceptionHandler();
      throw;
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::connect()
  {
    log ( Info() , "Attempting to create TCP connection to '" , ( **mEndpoint ).host_name() , "' port " , ( **mEndpoint ).service_name() , "." );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_connect ( *mSocket , *mEndpoint , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
    }
    while ( lErrorCode == boost::asio::error::would_block );

    if ( lErrorCode )
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO reported a Timeout in TCP callback" );
        throw exception::TcpTimeout();
      }

      log ( Error() , "ASIO reported an error: " , lErrorCode.message() );
      throw exception::ErrorInTcpCallback();
    }

    mSocket->set_option ( boost::asio::ip::tcp::no_delay ( true ) );
    log ( Info() , "TCP connection succeeded" );
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::write ( Buffers* aBuffers )
  {
    log ( Info() , ThisLocation(), ", Buffer: " , Pointer ( aBuffers ) );
    //     std::vector<uint32_t>::const_iterator lBegin( reinterpret_cast<uint32_t*>( aBuffers->getSendBuffer() ) );
    //     std::vector<uint32_t>::const_iterator lEnd = lBegin + (aBuffers->sendCounter()>>2);
    //     std::vector<uint32_t> lData;
    //     for( ; lBegin!=lEnd ; ++lBegin )
    //     {
    //       lData.push_back( ntohl ( *lBegin ) );
    //     }
    //     lBegin = lData.begin();
    //     lEnd = lData.end();
    //     HostToTargetInspector< 2 , 0 >().analyze ( lBegin , lEnd );
    mAsioSendBuffer.clear();
    mAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( aBuffers->sendCounter() ) , " bytes" );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
    boost::asio::async_write ( *mSocket , mAsioSendBuffer , boost::bind ( &TCP< InnerProtocol >::write_callback, this, aBuffers , _1 ) );
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::write_callback ( Buffers* aBuffers , const boost::system::error_code& aErrorCode )
  {
#ifdef FORCE_ONE_ASYNC_OPERATION_AT_A_TIME
    boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
    mReplyQueue.push_back ( aBuffers );

    if ( mReplyQueue.size() == 1 )
    {
      read ( aBuffers );
    }

    mDispatchQueue.pop_front();

    if ( mDispatchQueue.size() )
    {
      write ( mDispatchQueue.front() );
    }

#else
    read ( aBuffers );
#endif
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::read ( Buffers* aBuffers )
  {
    mAsioReplyBuffer.clear();
    mAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , aBuffers->replyCounter() ) );
    log ( Debug() , "Expecting " , Integer ( aBuffers->replyCounter() ) , " bytes in reply" );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
    boost::asio::async_read ( *mSocket , mAsioReplyBuffer ,  boost::asio::transfer_all(), boost::bind ( &TCP< InnerProtocol >::read_callback, this, aBuffers , _1 ) );
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::read_callback ( Buffers* aBuffers , const boost::system::error_code& aErrorCode )
  {
    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      if ( mSocket.unique() )
      {
        mSocket->close();
      }

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO reported a Timeout in TCP callback" );
        throw exception::TcpTimeout();
      }

      log ( Error() , "ASIO reported an error: " , Quote ( aErrorCode.message() ) , ". Attempting validation to see if we can get any more info." );
      ClientInterface::validate();
      throw exception::ErrorInTcpCallback();
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

    if ( !ClientInterface::validate() )
    {
      log ( Error() , "Validation function reported an error when processing buffer: " , Pointer ( aBuffers ) );
      throw exception::ValidationError ();
    }

#ifdef FORCE_ONE_ASYNC_OPERATION_AT_A_TIME
    boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
    mReplyQueue.pop_front();

    if ( mReplyQueue.size() )
    {
      read ( mReplyQueue.front() );
    }

#endif
  }

  template < typename InnerProtocol >
  void TCP< InnerProtocol >::CheckDeadline()
  {
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
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }






  template < typename InnerProtocol >
  void TCP< InnerProtocol >::Flush( )
  {
    bool lContinue ( true );

    do
    {
      boost::lock_guard<boost::mutex> lLock ( this->mMutex );
      lContinue = ( this->mDispatchedBuffers.size() );
    }
    while ( lContinue );
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

