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
// #include "uhal/logo.hpp"

#include <sys/time.h>

namespace uhal
{

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol , size_t nr_buffers_per_send >
  TCP< InnerProtocol, nr_buffers_per_send >::TCP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aUri.mHostname , aUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }



  template < typename InnerProtocol , size_t nr_buffers_per_send >
  TCP< InnerProtocol , nr_buffers_per_send >::TCP ( const TCP< InnerProtocol , nr_buffers_per_send >& aTCP ) :
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aTCP.mUri.mHostname , aTCP.mUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol , size_t nr_buffers_per_send >
  TCP< InnerProtocol, nr_buffers_per_send >& TCP< InnerProtocol , nr_buffers_per_send >::operator= ( const TCP< InnerProtocol , nr_buffers_per_send >& aTCP )
  {
    mEndpoint =  boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aTCP.mUri.mHostname , aTCP.mUri.mPort ) );

    try
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}
    }
    catch ( const std::exception& aExc )
    {
      exception::ASIOTcpError lExc;
      log ( lExc , "Error closing socket" );
      log ( lExc , "ASIO reported " , Quote ( aExc.what() ) );
      throw lExc;
    }

#ifdef RUN_ASIO_MULTITHREADED
    ClientInterface::returnBufferToPool ( mDispatchQueue );
    mDispatchQueue.clear();
    ClientInterface::returnBufferToPool ( mReplyQueue );
    mReplyQueue.clear();

    ClientInterface::returnBufferToPool ( mDispatchBuffers );
    mDispatchBuffers.clear();
    ClientInterface::returnBufferToPool ( mReplyBuffers );
    mReplyBuffers.clear();
    mPacketsInFlight = 0;
#endif

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }




  template < typename InnerProtocol , size_t nr_buffers_per_send >
  TCP< InnerProtocol , nr_buffers_per_send >::~TCP()
  {
    try
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      mIOservice.stop();
#ifdef RUN_ASIO_MULTITHREADED
      mDispatchThread.join();
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
      ClientInterface::returnBufferToPool ( mDispatchQueue );
      mDispatchQueue.clear();
      ClientInterface::returnBufferToPool ( mReplyQueue );
      mReplyQueue.clear();

      ClientInterface::returnBufferToPool ( mDispatchBuffers );
      mDispatchBuffers.clear();
      ClientInterface::returnBufferToPool ( mReplyBuffers );
      mReplyBuffers.clear();
#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }


  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
  {
    if ( mAsynchronousException )
    {
      log ( *mAsynchronousException , "Rethrowing Asynchronous Exception from " , ThisLocation() );
      mAsynchronousException->ThrowAsDerivedType();
    }

    if ( ! mSocket.is_open() )
    {
      connect();
    }

    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

      mDispatchQueue.push_back ( aBuffers );

      /*if ( mDispatchQueue.size() > 2 * this->getMaxNumberOfBuffers() )
      {
        this->returnBufferToPool( mDispatchQueue.front() );
        mDispatchQueue.pop_front();
      }*/

      if ( mDispatchBuffers.empty() && ( mDispatchQueue.size() >= nr_buffers_per_send ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
      {
        write ( );
      }

#else
      mDispatchBuffers = aBuffers;
      write ( );
#endif
    }
  }



  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::connect()
  {
    log ( Info() , "Attempting to create TCP connection to '" , mEndpoint->host_name() , "' port " , mEndpoint->service_name() , "." );
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_connect ( mSocket , mEndpoint , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
#ifndef RUN_ASIO_MULTITHREADED
      mIOservice.run_one();
#endif
    }
    while ( lErrorCode == boost::asio::error::would_block );

    if ( lErrorCode )
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      exception::TcpConnectionFailure lExc;

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( lExc , "ASIO TCP connection timed out" );
      }
      else
      {
        log ( lExc , "ASIO reported an error: " , lErrorCode.message() );
      }

      throw lExc;
    }

    mSocket.set_option ( boost::asio::ip::tcp::no_delay ( true ) );
    //    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
    //    mSocket.io_control ( lNonBlocking );
    log ( Info() , "TCP connection succeeded" );
  }



  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::write ( )
  {
#ifdef RUN_ASIO_MULTITHREADED
    assert ( mDispatchBuffers.empty() );
    assert ( ! mDispatchQueue.empty() );
 
    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( &mSendByteCounter , 4 ) );
    mSendByteCounter = 0;

    size_t lNrBuffersToSend = std::min(mDispatchQueue.size(), nr_buffers_per_send);
    mDispatchBuffers.reserve(lNrBuffersToSend);
    for ( size_t i = 0; i < lNrBuffersToSend; i++ )
    {
      mDispatchBuffers.push_back( mDispatchQueue.front() );
      mDispatchQueue.pop_front();

      const boost::shared_ptr<Buffers>& lBuffer = mDispatchBuffers.back();
      mSendByteCounter += lBuffer->sendCounter();
      lAsioSendBuffer.push_back ( boost::asio::const_buffer ( lBuffer->getSendBuffer() , lBuffer->sendCounter() ) );
    }

    assert ( ! mDispatchBuffers.empty() );

    log ( Debug() , "Sending " , Integer ( mSendByteCounter ) , " bytes from ", Integer ( mDispatchBuffers.size() ), " buffers" );
    mSendByteCounter = htonl ( mSendByteCounter );
#else
    if ( ! mDispatchBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mDispatchBuffers' was NULL" );
      return;
    }
    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    mSendByteCounter = htonl ( mDispatchBuffers->sendCounter() );
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( &mSendByteCounter , 4 ) );
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( mDispatchBuffers->getSendBuffer() , mDispatchBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( mDispatchBuffers->sendCounter() ) , " bytes" );
#endif

    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    while ( mDeadlineTimer.expires_from_now() < boost::posix_time::microseconds(600) )
    {
      log ( Fatal() , "N.B. Deadline timer just set to strange value in ", __func__, ". Expires_from_now is: ", mDeadlineTimer.expires_from_now(), " . Resetting ..." );
      mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    }
#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_write ( mSocket , lAsioSendBuffer , boost::bind ( &TCP< InnerProtocol , nr_buffers_per_send >::write_callback, this, _1 ) );
    mPacketsInFlight += mDispatchBuffers.size();
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_write ( mSocket , lAsioSendBuffer , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    write_callback ( lErrorCode );
#endif
  }



  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::write_callback ( const boost::system::error_code& aErrorCode )
  {
#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

    if ( mAsynchronousException )
    {
      return;
    }

    if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
    {
      exception::TcpTimeout* lExc = new exception::TcpTimeout();
      log ( *lExc , "ASIO reported a timeout in TCP write callback. Error code message is: ", Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
        log ( *lExc , "Error closing socket" );
        log ( *lExc , "ASIO reported " , Quote ( aExc.what() ) );
        mAsynchronousException = lExc;
        return;
      }

      exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
      log ( *lExc , "ASIO reported a write error: " , Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    if ( ! mReplyBuffers.empty() )
    {
      mReplyQueue.push_back ( mDispatchBuffers );
    }
    else
    {
      mReplyBuffers = mDispatchBuffers;
      read ( );
    }

    mDispatchBuffers.clear();

    if ( ( mDispatchQueue.size() >= nr_buffers_per_send ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write();
    }

#else
    mReplyBuffers = mDispatchBuffers;
    mDispatchBuffers.reset();
    read ( );
#endif
  }


  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::read ( )
  {
#ifdef RUN_ASIO_MULTITHREADED
    assert ( ! mReplyBuffers.empty() );
#else
    if ( ! mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }
#endif

    //     std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mReplyBuffers->getReplyBuffer() );
    //     std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    //     lAsioReplyBuffer.reserve ( lReplyBuffers.size() +1 );
    //     for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    //     {
    //       lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
    //     }
    //     lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( mReplyBuffers->getSpareSpace() , Buffers::mSpareSpaceSize ) );
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( &mReplyByteCounter , 4 ) );
    log ( Debug() , "Getting reply byte counter" );
    boost::asio::ip::tcp::endpoint lEndpoint;
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    while ( mDeadlineTimer.expires_from_now() < boost::posix_time::microseconds(600) )
    {
      log ( Fatal() , "N.B. Deadline timer just set to strange value in ", __func__, ". Expires_from_now is: ", mDeadlineTimer.expires_from_now(), " . Resetting ..." );
      mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    }
#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( 4 ), boost::bind ( &TCP< InnerProtocol , nr_buffers_per_send >::read_callback, this, _1 ) );
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( 4 ), boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    read_callback ( lErrorCode );
#endif
  }





  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::read_callback ( const boost::system::error_code& aErrorCode )
  {
    if ( mAsynchronousException )
    {
      return;
    }

#ifdef RUN_ASIO_MULTITHREADED
    assert ( ! mReplyBuffers.empty() );
#else
    if ( ! mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }
#endif

    if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
    {
      exception::TcpTimeout* lExc = new exception::TcpTimeout();
      log ( *lExc , "ASIO reported a timeout in TCP read callback. Error code message is: ", Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
        log ( *lExc , "Error closing socket" );
        log ( *lExc , "ASIO reported " , Quote ( aExc.what() ) );
        mAsynchronousException = lExc;
        return;
      }

      exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
      log ( *lExc , "ASIO reported a read error : " , Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    mReplyByteCounter = ntohl ( mReplyByteCounter );
    log ( Debug() , "Byte Counter says " , Integer ( mReplyByteCounter ) , " bytes are coming" );

    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
#ifdef RUN_ASIO_MULTITHREADED
    size_t lNrReplyBuffers = 1;
    uint32_t lExpectedReplyBytes = 0;
    for ( std::vector< boost::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.begin(); lBufIt != mReplyBuffers.end(); lBufIt++ )
    {
      lNrReplyBuffers += (*lBufIt)->getReplyBuffer().size();
      lExpectedReplyBytes += (*lBufIt)->replyCounter();
    }
    lAsioReplyBuffer.reserve ( lNrReplyBuffers );
    log ( Debug() , "Expecting " , Integer ( lExpectedReplyBytes ) , " bytes in reply, for ", Integer ( mReplyBuffers.size() ), " buffers" );

    for ( std::vector< boost::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.begin(); lBufIt != mReplyBuffers.end(); lBufIt++ )
    {
      std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( (*lBufIt)->getReplyBuffer() );
      for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
      {
        lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
      }
    }
#else
    log ( Debug() , "Expecting " , Integer ( mReplyBuffers->replyCounter() ) , " bytes in reply" );
    std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mReplyBuffers->getReplyBuffer() );
    lAsioReplyBuffer.reserve ( lReplyBuffers.size() +1 );
    
    for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    {
      lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
    }
#endif

    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( mReplyByteCounter ), lErrorCode );
    //     do
    //     {
    //       std::cout << "." << std::flush;
    // #ifndef RUN_ASIO_MULTITHREADED
    //       mIOservice.run_one();
    // #endif
    //     }
    //     while ( lErrorCode == boost::asio::error::would_block );

    if ( lErrorCode && ( lErrorCode != boost::asio::error::eof ) )
    {
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        exception::TcpTimeout* lExc = new exception::TcpTimeout();
        log ( *lExc , "ASIO reported an error: " , Quote ( lErrorCode.message() ) );
        log ( *lExc , "ASIO reported a timeout in TCP callback" );
        mAsynchronousException = lExc;
        return;
      }

      exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
      log ( *lExc , "ASIO reported an error: " , Quote ( lErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

#ifdef RUN_ASIO_MULTITHREADED
    for ( std::vector< boost::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.begin(); lBufIt != mReplyBuffers.end(); lBufIt++ )
    {
      try
      {
        mAsynchronousException = ClientInterface::validate ( *lBufIt ); //Control of the pointer has been passed back to the client interface
      }
      catch ( exception::exception& aExc )
      {
        mAsynchronousException = new exception::ValidationError ();
      }

      if ( mAsynchronousException )
      {
        return;
      }
    }

    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

    mPacketsInFlight -= mReplyBuffers.size();

    if ( mReplyQueue.size() )
    {
      mReplyBuffers = mReplyQueue.front();
      mReplyQueue.pop_front();
      read();
    }
    else
    {
      mReplyBuffers.clear();
    }

    if ( mDispatchBuffers.empty() && (mDispatchQueue.size() > nr_buffers_per_send) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write();
    }

#else
    try
    {
      mAsynchronousException = ClientInterface::validate ( mReplyBuffers ); //Control of the pointer has been passed back to the client interface
    }
    catch ( exception::exception& aExc )
    {
      mAsynchronousException = new exception::ValidationError ();
    }
    if ( mAsynchronousException )
    {
      return;
    }

    mReplyBuffers.reset();
#endif
  }



  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::CheckDeadline()
  {
    boost::lock_guard<boost::mutex> lLock ( this->mTransportLayerMutex );

    // Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
    if ( mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
    {
      // SETTING THE EXCEPTION HERE CAN APPEAR AS A TIMEOUT WHEN NONE ACTUALLY EXISTS
      log ( Warning() , "Closing socket since deadline has passed" );
      // The deadline has passed. The socket is closed so that any outstanding asynchronous operations are cancelled.
      mSocket.close();

      // There is no longer an active deadline. The expiry is set to positive infinity so that the actor takes no action until a new deadline is set.
      mDeadlineTimer.expires_at ( boost::posix_time::pos_infin );
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::Flush( )
  {
#ifdef RUN_ASIO_MULTITHREADED
    bool lContinue ( true );

    while ( lContinue )
    {
      if ( mAsynchronousException )
      {
        mAsynchronousException->ThrowAsDerivedType();
      }

      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
      if ( mDispatchBuffers.empty() && mReplyBuffers.empty() )
      {
        if ( mDispatchQueue.size() )
        { 
          write();
        }
        else
        {
          lContinue = false;
        }
      }
    }

    assert ( mDispatchBuffers.empty() );
    assert ( mDispatchQueue.empty() );
    assert ( mReplyBuffers.empty() );
    assert ( mReplyQueue.empty() );
#endif
  }



  template < typename InnerProtocol , size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::dispatchExceptionHandler()
  {
    log ( Warning() , "Closing Socket since exception detected." );

    if ( mSocket.is_open() )
    {
      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( ... )
        {}
    }

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
      ClientInterface::returnBufferToPool ( mDispatchQueue );
      mDispatchQueue.clear();
      ClientInterface::returnBufferToPool ( mReplyQueue );
      mReplyQueue.clear();
      mPacketsInFlight = 0;
      ClientInterface::returnBufferToPool ( mDispatchBuffers );
      mDispatchBuffers.clear();
      ClientInterface::returnBufferToPool ( mReplyBuffers );
      mReplyBuffers.clear();
#else
      mDispatchBuffers.reset();
      mReplyBuffers.reset();
#endif
      mSendByteCounter = 0;
      mReplyByteCounter = 0;
    }

    InnerProtocol::dispatchExceptionHandler();
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

