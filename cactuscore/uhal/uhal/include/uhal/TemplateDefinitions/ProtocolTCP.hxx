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
#include "boost/date_time/posix_time/posix_time.hpp"


#include "uhal/IPbusInspector.hpp"
// #include "uhal/logo.hpp"

#include <sys/time.h>

namespace uhal
{

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  TCP< InnerProtocol, nr_buffers_per_send >::TCP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mIOservice ( ),
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aUri.mHostname , aUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
    mFlushStarted ( false ),
    mFlushDone ( true ),
#endif
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  TCP< InnerProtocol , nr_buffers_per_send >::TCP ( const TCP< InnerProtocol , nr_buffers_per_send >& aTCP ) :
    mIOservice ( ),
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aTCP.mUri.mHostname , aTCP.mUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
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
      log ( lExc , "Error " , Quote ( aExc.what() ) , " encountered when closing TCP socket for URI: ", this->uri() );
      throw lExc;
    }

#ifdef RUN_ASIO_MULTITHREADED
    ClientInterface::returnBufferToPool ( mDispatchQueue );
    ClientInterface::returnBufferToPool ( mReplyQueue );
    ClientInterface::returnBufferToPool ( mDispatchBuffers );
    ClientInterface::returnBufferToPool ( mReplyBuffers );
    mPacketsInFlight = 0;
#endif

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }




  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
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
      ClientInterface::returnBufferToPool ( mDispatchQueue );
      ClientInterface::returnBufferToPool ( mReplyQueue );
      ClientInterface::returnBufferToPool ( mDispatchBuffers );
      ClientInterface::returnBufferToPool ( mReplyBuffers );
#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
  {
#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
#endif

    if ( mAsynchronousException )
    {
      log ( *mAsynchronousException , "Rethrowing Asynchronous Exception from " , ThisLocation() );
      mAsynchronousException->ThrowAsDerivedType();
    }

    if ( ! mSocket.is_open() )
    {
      connect();
    }

#ifdef RUN_ASIO_MULTITHREADED
    mFlushStarted = false;
    mDispatchQueue.push_back ( aBuffers );

    if ( mDispatchBuffers.empty() && ( mDispatchQueue.size() >= nr_buffers_per_send ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write ( );
    }
#else
    mDispatchBuffers = aBuffers;
    write ( );
#endif
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::connect()
  {
    log ( Info() , "Attempting to create TCP connection to '" , mEndpoint->host_name() , "' port " , mEndpoint->service_name() , "." );
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_connect ( mSocket , mEndpoint , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

#ifdef RUN_ASIO_MULTITHREADED
    // Unlock mutex whilst connect runs in other thread, to avoid deadlock in case timeout occurs
    mTransportLayerMutex.unlock();
    do
    {
    }
    while ( lErrorCode == boost::asio::error::would_block );
    mTransportLayerMutex.lock();
#else
    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );
#endif

    if ( lErrorCode )
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      exception::TcpConnectionFailure lExc;
      std::ostringstream oss;

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        oss << "Timeout (" << this->getBoostTimeoutPeriod().total_milliseconds() << " milliseconds) occurred when connecting to ";
      }
      else if ( lErrorCode == boost::asio::error::connection_refused )
      {
        oss << "Connection refused for ";
      }
      else
      {
        oss << "Error \"" << lErrorCode.message() << "\" encountered when connecting to ";
      }

      log ( lExc , oss.str() , ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
            " at ", Quote ( this->mUri.mHostname + ":" + this->mUri.mPort ) , ". URI=" , Quote ( this->uri() ) );
      throw lExc;
    }

    mSocket.set_option ( boost::asio::ip::tcp::no_delay ( true ) );
    //    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
    //    mSocket.io_control ( lNonBlocking );
    log ( Info() , "TCP connection succeeded" );
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::write ( )
  {
    NotifyConditionalVariable ( false );

#ifdef RUN_ASIO_MULTITHREADED
    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( &mSendByteCounter , 4 ) );
    mSendByteCounter = 0;
    std::size_t lNrBuffersToSend = std::min ( mDispatchQueue.size(), nr_buffers_per_send );
    mDispatchBuffers.reserve ( lNrBuffersToSend );

    for ( std::size_t i = 0; i < lNrBuffersToSend; i++ )
    {
      mDispatchBuffers.push_back ( mDispatchQueue.front() );
      mDispatchQueue.pop_front();
      const boost::shared_ptr<Buffers>& lBuffer = mDispatchBuffers.back();
      mSendByteCounter += lBuffer->sendCounter();
      lAsioSendBuffer.push_back ( boost::asio::const_buffer ( lBuffer->getSendBuffer() , lBuffer->sendCounter() ) );
    }

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

    // Patch for suspected bug in using boost asio with boost python; see https://svnweb.cern.ch/trac/cactus/ticket/323#comment:7
    while ( mDeadlineTimer.expires_from_now() < boost::posix_time::microseconds ( 600 ) )
    {
      log ( Debug() , "Resetting deadline timer since it just got set to strange value, likely due to a bug within boost (expires_from_now was: ", mDeadlineTimer.expires_from_now() , ")." );
      mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    }

#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_write ( mSocket , lAsioSendBuffer , boost::bind ( &TCP< InnerProtocol , nr_buffers_per_send >::write_callback, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred ) );
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



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::write_callback ( const boost::system::error_code& aErrorCode , std::size_t aBytesTransferred )
  {
#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
    mSendByteCounter = ntohl ( mSendByteCounter );

    if ( mAsynchronousException )
    {
      NotifyConditionalVariable ( true );
      return;
    }

    if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
    {
      mAsynchronousException = new exception::TcpTimeout();
      log ( *mAsynchronousException , "Timeout (" , Integer ( this->getBoostTimeoutPeriod().total_milliseconds() ) , " milliseconds) occurred for send to ",
            ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: ", this->uri() );

      if ( aErrorCode && aErrorCode != boost::asio::error::operation_aborted )
      {
        log ( *mAsynchronousException , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
      }

      NotifyConditionalVariable ( true );
      return;
    }

    if ( ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) ) || ( aBytesTransferred != ( mSendByteCounter+4 ) ) )
    {
      mAsynchronousException = new exception::ASIOTcpError();
      log ( *mAsynchronousException , "Error ", Quote ( aErrorCode.message() ) , " encountered during send to ",
            ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: " , this->uri() );

      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        log ( *mAsynchronousException , "Error closing TCP socket following the ASIO send error" );
      }

      if ( aBytesTransferred != ( mSendByteCounter + 4 ) )
      {
        log ( *mAsynchronousException , "Attempted to send " , Integer ( mSendByteCounter ) , " bytes to ",
             ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
             " with URI "  , Quote ( this->uri() ) , ", but only sent " , Integer ( aBytesTransferred ) , " bytes" );
      }

      NotifyConditionalVariable ( true );
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

    if ( ( mDispatchQueue.size() >= (mFlushStarted ? 1 : nr_buffers_per_send) ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write();
    }

#else
    mReplyBuffers = mDispatchBuffers;
    mDispatchBuffers.reset();
    read ( );
#endif
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::read ( )
  {
#ifndef RUN_ASIO_MULTITHREADED
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
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( &mReplyByteCounter , 4 ) );
    log ( Debug() , "Getting reply byte counter" );
    boost::asio::ip::tcp::endpoint lEndpoint;
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );

    // Patch for suspected bug in using boost asio with boost python; see https://svnweb.cern.ch/trac/cactus/ticket/323#comment:7
    while ( mDeadlineTimer.expires_from_now() < boost::posix_time::microseconds ( 600 ) )
    {
      log ( Debug() , "Resetting deadline timer since it just got set to strange value, likely due to a bug within boost (expires_from_now was: ", mDeadlineTimer.expires_from_now() , ")." );
      mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    }

#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( 4 ), boost::bind ( &TCP< InnerProtocol , nr_buffers_per_send >::read_callback, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred ) );
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





  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::read_callback ( const boost::system::error_code& aErrorCode , std::size_t aBytesTransferred )
  {
#ifndef RUN_ASIO_MULTITHREADED
    if ( ! mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }
#endif

    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
#endif
      if ( mAsynchronousException )
      {
        NotifyConditionalVariable ( true );
        return;
      }

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        exception::TcpTimeout* lExc = new exception::TcpTimeout();
        log ( *lExc , "Timeout (" , Integer ( this->getBoostTimeoutPeriod().total_milliseconds() ) , " milliseconds) occurred for receive from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: ", this->uri() );

        if ( aErrorCode && aErrorCode != boost::asio::error::operation_aborted )
        {
          log ( *lExc , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
        }

        mAsynchronousException = lExc;
        NotifyConditionalVariable ( true );
        return;
      }
    }

    if ( ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) ) || ( aBytesTransferred != 4 ) )
    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
#endif
      mAsynchronousException = new exception::ASIOTcpError();

      if ( aErrorCode )
      {
        log ( *mAsynchronousException , "Error ", Quote ( aErrorCode.message() ) , " encountered during receive from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: " , this->uri() );
      }

      if ( aBytesTransferred != 4 )
      {
        log ( *mAsynchronousException, "Expected to receive 4-byte header in async read from ",
             ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
             " with URI "  , Quote ( this->uri() ) , ", but only received " , Integer ( aBytesTransferred ) , " bytes" );
      }

      try
      {
        mSocket.close();
        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        log ( *mAsynchronousException , "Error closing socket following ASIO read error" );
      }

      NotifyConditionalVariable ( true );
      return;
    }

    mReplyByteCounter = ntohl ( mReplyByteCounter );
    log ( Debug() , "Byte Counter says " , Integer ( mReplyByteCounter ) , " bytes are coming" );
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
#ifdef RUN_ASIO_MULTITHREADED
    std::size_t lNrReplyBuffers = 1;
    uint32_t lExpectedReplyBytes = 0;

    for ( std::vector< boost::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.begin(); lBufIt != mReplyBuffers.end(); lBufIt++ )
    {
      lNrReplyBuffers += ( *lBufIt )->getReplyBuffer().size();
      lExpectedReplyBytes += ( *lBufIt )->replyCounter();
    }

    lAsioReplyBuffer.reserve ( lNrReplyBuffers );
    log ( Debug() , "Expecting " , Integer ( lExpectedReplyBytes ) , " bytes in reply, for ", Integer ( mReplyBuffers.size() ), " buffers" );

    for ( std::vector< boost::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.begin(); lBufIt != mReplyBuffers.end(); lBufIt++ )
    {
      std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( ( *lBufIt )->getReplyBuffer() );

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
    std::size_t lBytesTransferred = boost::asio::read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( mReplyByteCounter ), lErrorCode );

    if ( ( lErrorCode && ( lErrorCode != boost::asio::error::eof ) ) || ( lBytesTransferred != mReplyByteCounter ) )
    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
#endif
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        mAsynchronousException = new exception::TcpTimeout();
        log ( *mAsynchronousException , "Timeout (" , Integer ( this->getBoostTimeoutPeriod().total_milliseconds() ) , " milliseconds) occurred for receive from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: ", this->uri() );
      }
      else
      {
        mAsynchronousException = new exception::ASIOTcpError();
        log ( *mAsynchronousException , "Error ", Quote ( aErrorCode.message() ) , " encountered during receive from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: " , this->uri() );
      }

      if ( lBytesTransferred != mReplyByteCounter )
      {
        log ( *mAsynchronousException, "Expected to receive " , Integer ( mReplyByteCounter ) , " bytes in read from ",
             ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
             " with URI "  , Quote ( this->uri() ) , ", but only received " , Integer ( lBytesTransferred ) , " bytes" );
      }

      NotifyConditionalVariable ( true );
      return;
    }

#ifdef RUN_ASIO_MULTITHREADED

    for ( std::vector< boost::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.begin(); lBufIt != mReplyBuffers.end(); lBufIt++ )
    {
      try
      {
        if ( uhal::exception::exception* lExc =  ClientInterface::validate ( *lBufIt ) ) //Control of the pointer has been passed back to the client interface
        {
          boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
          mAsynchronousException = lExc;
        }
      }
      catch ( exception::exception& aExc )
      {
        boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
        mAsynchronousException = new exception::ValidationError ();
        log ( *mAsynchronousException , "Exception caught during reply validation for TCP device with URI " , Quote ( this->uri() ) , "; what returned: " , Quote ( aExc.what() ) );
      }

      if ( mAsynchronousException )
      {
        NotifyConditionalVariable ( true );
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

    if ( mDispatchBuffers.empty() && ( mDispatchQueue.size() >= (mFlushStarted ? 1 : nr_buffers_per_send) ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write();
    }

    if ( mDispatchBuffers.empty() && mReplyBuffers.empty() )
    {
      mDeadlineTimer.expires_from_now ( boost::posix_time::seconds(60) );
      NotifyConditionalVariable ( true );
    }

#else

    try
    {
      mAsynchronousException = ClientInterface::validate ( mReplyBuffers ); //Control of the pointer has been passed back to the client interface
    }
    catch ( exception::exception& aExc )
    {
      mAsynchronousException = new exception::ValidationError ();
      log ( *mAsynchronousException , "Exception caught during reply validation for URI: " , Quote ( this->uri() ) , "; what returned: " , Quote ( aExc.what() ) );
    }

    if ( mAsynchronousException )
    {
      return;
    }

    mReplyBuffers.reset();
#endif
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::CheckDeadline()
  {
#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( this->mTransportLayerMutex );
#endif

    // Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
    if ( mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
    {
      // SETTING THE EXCEPTION HERE CAN APPEAR AS A TIMEOUT WHEN NONE ACTUALLY EXISTS
#ifdef RUN_ASIO_MULTITHREADED
      if (  mDispatchBuffers.size() || mReplyBuffers.size() )
      {
        log ( Warning() , "Closing TCP socket for device with URI " , Quote ( this->uri() ) , " since deadline has passed" );
      }
      else
      {
        log ( Debug() , "Closing TCP socket for device with URI " , Quote ( this->uri() ) , " since no communication in 60 seconds" );
      }
#endif
      // The deadline has passed. The socket is closed so that any outstanding asynchronous operations are cancelled.
      mSocket.close();
      // There is no longer an active deadline. The expiry is set to positive infinity so that the actor takes no action until a new deadline is set.
      mDeadlineTimer.expires_at ( boost::posix_time::pos_infin );
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::Flush( )
  {
#ifdef RUN_ASIO_MULTITHREADED
    {
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
      mFlushStarted = true;

      if ( mDispatchQueue.size() && mDispatchBuffers.empty() )
      {
        write();
      }
    }

    WaitOnConditionalVariable();

    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex ); 

    if ( mAsynchronousException )
    {
      mAsynchronousException->ThrowAsDerivedType();
    }
#endif
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
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

    NotifyConditionalVariable ( true );

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

#ifdef RUN_ASIO_MULTITHREADED
    ClientInterface::returnBufferToPool ( mDispatchQueue );
    ClientInterface::returnBufferToPool ( mReplyQueue );
    mPacketsInFlight = 0;
    ClientInterface::returnBufferToPool ( mDispatchBuffers );
    ClientInterface::returnBufferToPool ( mReplyBuffers );
#else
    mDispatchBuffers.reset();
    mReplyBuffers.reset();
#endif
    mSendByteCounter = 0;
    mReplyByteCounter = 0;

    InnerProtocol::dispatchExceptionHandler();
  }


  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::NotifyConditionalVariable ( const bool& aValue )
  {
#ifdef RUN_ASIO_MULTITHREADED
    {
      boost::lock_guard<boost::mutex> lLock ( mConditionalVariableMutex );
      mFlushDone = aValue;
    }
    mConditionalVariable.notify_one();
#endif
  }

  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::WaitOnConditionalVariable()
  {
#ifdef RUN_ASIO_MULTITHREADED
    boost::unique_lock<boost::mutex> lLock ( mConditionalVariableMutex );

    while ( !mFlushDone )
    {
      mConditionalVariable.wait ( lLock );
    }

#endif
  }

}

